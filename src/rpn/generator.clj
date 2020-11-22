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
(ns rpn.generator
  "Code generator."
  (:require [rpn.ast :as ast])
  (:require [rpn.code :as code]))

(def ^:private operator-codes
  {:add (code/add-code 2)
   :subtract (code/subtract-code 2)
   :multiply (code/multiply-code 2)
   :divide (code/divide-code 2)
   :min (code/min-code 2)
   :max (code/max-code 2)
   :modulo code/modulo-code
   :power code/power-code})

(defn- generate
  ([ast]
    (let [[codes syms] (generate ast [[] #{}])]
      (concat
        (map #(code/declare-symbol-code %) (sort syms))
        codes)))
  ([ast state]
    (let [k (ast/kind ast)]
      (cond
        (= k :symbol)
          (let [[cs syms] state]
            [(conj cs (code/push-symbol-code (ast :name))) (conj syms (ast :name))])
        (= k :number)
          (let [[cs syms] state]
            [(conj cs (code/push-code (ast :value))) syms])
        (contains? operator-codes k)
          (let [[cs syms] (generate (ast :right) (generate (ast :left) state))]
            [(conj cs (operator-codes k)) syms])))))

(defn generator
  "A code generator that transforms a syntax tree into a sequence of unoptimized
  instructions."
  [ast]
  (generate ast))
