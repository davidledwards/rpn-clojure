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

(defn- combine-dynamic-operators-revisions [codes]
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

(defn- combine-dynamic-operators [codes]
  (revise codes (combine-dynamic-operators-revisions codes)))

(defn- flatten-dynamic-operators-revisions [codes]
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

(defn- flatten-dynamic-operators [codes]
  (revise codes (flatten-dynamic-operators-revisions codes)))

(defn- analyze
  ([codes]
    (analyze 0 codes ()))
  ([pos codes stack]
    (let [c (first codes)]
      (cond
        (code/declare-symbol-code? c)
          (analyze (inc pos) (rest codes) stack)
        (code/push-symbol-code? c)
          (analyze (inc pos) (rest codes) (cons nil stack))
        (code/push-code? c)
          (analyze (inc pos) (rest codes) (cons {:pos pos :value (c :value)} stack))
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
                  (analyze (inc pos) (rest codes) stack)
                  revs))))
        (nil? c)
          {}
        :else
          (analyze (inc pos) (rest codes) stack)))))

(defn- evaluate-literal-expressions [codes]
  (let [revs (analyze codes)]
    (if (empty? revs)
      codes
      (recur (revise codes revs)))))

(def ^:private optimizations
  (comp
    evaluate-literal-expressions
    flatten-dynamic-operators
    combine-dynamic-operators))

(defn optimizer [codes]
  (let [cs (optimizations codes)]
    (if (< (count cs) (count codes))
      (recur cs)
      cs)))
