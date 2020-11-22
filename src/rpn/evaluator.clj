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
(ns rpn.evaluator
  "RPN evaluator."
  (:require [rpn.code :as code]))

(def operators
  {:add
    #(+ %1 %2)
   :subtract
    #(- %1 %2)
   :multiply
    #(* %1 %2)
   :divide
    #(/ %1 (double %2))
   :min
    #(min %1 %2)
   :max
    #(max %1 %2)
   :modulo
    #(try (mod %1 %2) (catch NumberFormatException _ ##NaN))
   :power
    #(Math/pow %1 %2)})

(defn evaluator
  "An evaluator that computes the result of an instruction sequence.
  
  `resolver` is a single-argument function that returns the value of a symbol, the type
  of which should be double. `codes` is the sequence of instructions."
  [resolver codes]
  (loop [cs codes
         stack nil
         syms {}]
    (let [c (first cs)]
      (cond
        (code/declare-symbol-code? c)
          (if-let [v (resolver (c :name))]
            (recur (rest cs) stack (conj syms {(c :name) v}))
            (throw (Exception. (str (c :name) ": symbol not bound"))))
        (code/push-symbol-code? c)
          (if-let [v (syms (c :name))]
            (recur (rest cs) (cons v stack) syms)
            (throw (Exception. (str (c :name) ": symbol not bound"))))
        (code/push-code? c)
          (recur (rest cs) (cons (c :value) stack) syms)
        (code/operator? c)
          (let [vs (take (c :argn) stack)]
            (if (= (count vs) (c :argn))
              (recur
                (rest cs)
                (cons
                  (reduce
                    #((operators (code/kind c)) %1 %2)
                    (reverse vs))
                  (drop (c :argn) stack))
                syms)
              (throw (Exception. (str "evaluator stack underflow" (prn c) (prn stack) (pr cs))))))
        c
          (recur (rest cs) stack syms)
        :else
          (if (= (count stack) 1)
            (first stack)
            (throw (Exception. (str "evaluator stack size should be 1, but is " (count stack)))))))))
