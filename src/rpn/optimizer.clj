;;;
;;; Copyright 2020 David Edwards
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
(ns rpn.optimizer
  "Optimizer."
  (:require [rpn.code :as code])
  (:require [rpn.evaluator :as evaluator]))

(def ^:private dynamic-ctors
  {:add code/add-code
   :subtract code/subtract-code
   :multiply code/multiply-code
   :divide code/divide-code
   :min code/min-code
   :max code/max-code})

(defn- dynamic-code [kind argn]
  ((dynamic-ctors kind) argn))

(def ^:private operator-ctors
  (conj
    {:modulo (fn [& _] code/modulo-code)
     :power (fn [& _] code/power-code)}
    dynamic-ctors))

(defn- operator-code [kind argn]
  ((operator-ctors kind) argn))

(defn- revise [codes revs]
  (->>
    (reduce
      (fn [state code]
        (let [[pos cs] state]
          [(inc pos)
            (if-let [c (revs pos)]
              (if (code/nop-code? c)
                cs
                (conj cs c))
              (conj cs code))]))
      [0 []] codes)
    (#(seq (last %)))))

(defn- combine-operators-revisions [codes]
  (->>
    (reduce
      (fn [state code]
        (let [[pos frames revs] state]
          (cons (inc pos)
            (cond
              (or (code/push-symbol-code? code) (code/push-code? code))
                [(cons nil frames) revs]
              (code/multiary-operator? code)
                (->>
                  (let [f (first (drop (dec (code :argn)) frames))]
                    (if (and f (code/same-kind? (f :code) code))
                      (let [new-code (dynamic-code
                                      (code/kind code)
                                      (dec (+ (code :argn) ((f :code) :argn))))]
                        [new-code (assoc revs (f :pos) code/nop-code pos new-code)])
                      [code revs]))
                  (#(let [[new-code revs] %]
                    [(->>
                      (drop (code :argn) frames)
                      (cons {:pos pos :code new-code})) revs])))
              (code/operator? code)
                [(->>
                  (drop (code :argn) frames)
                  (cons nil)) revs]
              :else
                [frames revs]))))
      [0 nil {}] codes)
    (#(last %))))

(defn combine-operators
  "An optimization that combines a series of identical operations as they appear in the
  original source.

  Consider the following input: `x + y + z`
  
  The parser generates an AST that first evaluates `x + y`, then evaluates the result of
  that expression and `z`. The corresponding bytecode follows:
  ```
  push x
  push y
  add 2
  push z
  add 2
  ```
  
  The `add 2` instruction tells the interpreter to pop `2` elements from the evaluation
  stack, compute the sum, and push the result onto the stack. Since the `add` instruction
  can operate on any number of arguments, both `add` operations can be combined into a
  single instruction:
  ```
  push x
  push y
  push z
  add 3
  ```
  
  A slightly more complicated example illustrates the same principle. Consider the input,
  `a + (b * c) + d`, and the corresponding bytecode:
  ```
  push a
  push b
  push c
  mul 2
  add 2
  push d
  add 2
  ```
  
  Similar to the first scenario, both `add` operations can be combined even though the
  intervening expression `b * c` exists. Note that adjacency of instructions is not
  relevant, but rather the equivalence of the evaluation stack frame depth. In other
  words, all operations of the same type at the same frame can be combined into a single
  operation.
  
  The algorithm works by simulating execution using an evaluation stack, maintaining a
  set of instructions that are candidates for elimination. If another instruction at the
  same frame depth is encountered, the original instruction is replaced with a `nop` and
  the current instruction modified to evaluate additional elements on the stack. Once
  all instructions have been evaluated, the set of revisions are applied, resulting in
  a new sequence of instructions."
  [codes]
  (revise codes (combine-operators-revisions codes)))

(defn- flatten-operators-revisions [codes]
  (->>
    (reduce
      (fn [state code]
        (let [[pos prior revs] state]
          (cons (inc pos)
            (if (and (code/multiary-operator? code) (code/associative-operator? code))
              (if (code/same-kind? code prior)
                (let [new-code (dynamic-code
                                (code/kind code)
                                (dec (+ (code :argn) (prior :argn))))]
                  [new-code (assoc revs (dec pos) code/nop-code pos new-code)])
                [code revs])
              [nil revs]))))
      [0 nil {}] codes)
    (#(last %))))

(defn flatten-operators
  "An optimization that flattens identical operations adjacent to each other in the
  instruction sequence.
  
  This optimization is similar to [[combine-operators]] in that operations are
  essentially combined, but instead it looks for special cases in which identical operations
  occur in adjacent frames on the evaluation stack.
  
  Consider the input, `x * (y * z)`, and the corresponding bytecode:
  ```
  push x
  push y
  push z
  mul 2
  mul 2
  ```
  
  Note that both `mul` instructions occur in adjacent positions. At first glance, it may
  appear as though [[combine-operators]] would eliminate one of the operations, but
  each occurs at a different frame on the evaluation stack.
  
  The intuition behind this optimization is that the first `mul 2` would push its result
  onto the stack, only to be removed for evaluation by the second `mul 2` instruction. So,
  rather than performing an intermediate calculation, the first can be eliminated in lieu of
  a single `mul 3` instruction. In general, any number of adjacent identical instructions
  can be reduced to a single instruction.
  
  One may notice that this phase optimizes right-to-left evaluation scenarios, but only for
  those operators with the associative property, i.e. evaluation can be left-to-right or
  right-to-left. This becomes more clear with another example: `a * (b * (c * d))`.
  The original instruction sequence follows:
  ```
  push a
  push b
  push c
  push d
  mul 2
  mul 2
  mul 2
  ```
  
  In essence, the parentheses are being removed and evaluated in a left-to-right manner
  by eliminating all but the last `mul` instruction:
  ```
  push a
  push b
  push c
  push d
  mul 4
  ```
  
  The algorithm works by stepping through each associative operator instruction, finding
  adjacent identical pairs, and eliminating all but the final instruction, which is then
  modified to reflect the combined number of arguments."
  [codes]
  (revise codes (flatten-operators-revisions codes)))

(defn- evaluate-literals-revisions
  ([codes]
    (evaluate-literals-revisions 0 codes ()))
  ([pos codes stack]
    (let [c (first codes)]
      (cond
        (code/declare-symbol-code? c)
          (recur (inc pos) (rest codes) stack)
        (code/push-symbol-code? c)
          (recur (inc pos) (rest codes) (cons nil stack))
        (code/push-code? c)
          (recur (inc pos) (rest codes) (cons {:pos pos :value (c :value)} stack))
        (code/operator? c)
          (->>
            [(let [nums (filter some? (reverse (take (c :argn) stack)))]
              (if (or
                    (and (> (count nums) 1) (code/commutative-operator? c))
                    (= (count nums) (c :argn)))
                (conj
                  (reduce
                    (fn [revs num]
                      (conj revs {(num :pos) code/nop-code}))
                    {} (drop-last nums))
                  {((last nums) :pos)
                    (code/push-code
                      (reduce (evaluator/operators (code/kind c)) (map #(% :value) nums)))}
                  {pos
                    (if (= (count nums) (c :argn))
                      code/nop-code
                      (operator-code
                        (code/kind c)
                        (inc (- (c :argn) (count nums)))))})
                {}))
              (cons nil (drop (c :argn) stack))]
            (#(let [[revs stack] %]
                (if (empty? revs)
                  (evaluate-literals-revisions (inc pos) (rest codes) stack)
                  revs))))
        (nil? c)
          {}
        :else
          (recur (inc pos) (rest codes) stack)))))

(defn evaluate-literals
  "An optimization that evaluates literal expressions.
  
  This optimization finds expressions containing only literal values and reduces them to a
  single value, thereby eliminating the need for the interpreter to perform the computation.

  Consider the input, `x + 1 + y + 2`, which produces the following sequence of
  unoptimized instructions:
  ```
  push x
  push 1
  add 2
  push y
  add 2
  push 2
  add 2
  ```
  
  Applying the [[combine-operators]] optimization produces the following:
  ```
  push x
  push 1
  push y
  push 2
  add 4
  ```
  
  In either case, there is still opportunity to further optimize. Had the original input
  been written as `x + y + 1 + 2`, it becomes more clear that `1 + 2` could be replaced
  with `3`. The purpose of this optimization phase is to find such expressions and reduce
  them to a single value.

  In the latter optimized case above, applying this optimization reduces the instruction
  sequence to the following:
  ```
  push x
  push y
  push 3
  add 3
  ```
  
  The algorithm works by simulating execution of the instruction sequence using an evaluation
  stack, though recording only literal values. As operations are encountered, the optimizer
  peeks into the evaluation stack to determine if two or more literals are present, and if so,
  eliminates the `push` instruction corresponding to each literal in lieu of a single `push`.
  When an optimization is detected, the evaluation terminates and revisions are applied to
  the original sequence of instructions. This process repeats itself until a complete
  evaluation yields no new optimizations.
  
  Note that an expression consisting entirely of literals will always be reduced to a single
  `push` instruction containing the computed value."
  [codes]
  (let [revs (evaluate-literals-revisions codes)]
    (if (empty? revs)
      codes
      (recur (revise codes revs)))))

(def ^:private optimizations
  (comp
    evaluate-literals
    flatten-operators
    combine-operators))

(defn optimizer
  "An optimizer that transforms a sequence of instructions into another sequence of
  instructions."
  [codes]
  (let [cs (optimizations codes)]
    (if (< (count cs) (count codes))
      (recur cs)
      cs)))
