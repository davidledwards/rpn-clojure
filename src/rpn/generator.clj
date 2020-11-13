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
  (:require [rpn.ast :as ast])
  (:require [rpn.code :as code]))

(defn- generate
  ([ast]
   (let [[codes syms] (generate ast [[] #{}])]
     (concat (map #(code/declare-symbol-code %) (sort syms)) codes)))
  ([ast state]
    (case (ast/kind ast)
      :symbol
        (let [[codes syms] state]
          [(conj codes (code/push-symbol-code (ast :name))) (conj syms (ast :name))])
      :number
        (let [[codes syms] state]
          [(conj codes (code/push-code (ast :value))) syms])
      :add
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes (code/add-code 2)) syms])
      :subtract
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes (code/subtract-code 2)) syms])
      :multiply
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes (code/multiply-code 2)) syms])
      :divide
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes (code/divide-code 2)) syms])
      :min
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes (code/min-code 2)) syms])
      :max
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes (code/max-code 2)) syms])
      :modulo
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes code/modulo-code) syms])
      :power
        (let [[codes syms] (generate (ast :right) (generate (ast :left) state))]
          [(conj codes code/power-code) syms]))))

(defn generator [ast]
  (generate ast))
