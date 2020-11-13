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
(ns rpn.ast
  (:require [clojure.string :as string]))

(def kinds
  #{:symbol
    :number
    :add
    :subtract
    :multiply
    :divide
    :modulo
    :power
    :min
    :max})

(defn- AST [kind]
  {:kind kind})

(defn- binary-AST [kind l r]
  (conj (AST kind) {:left l :right r}))

(defn symbol-AST [name]
  (conj
    (AST :symbol)
    {:name (if (string? name) name (str name))}))

(defn number-AST [value]
  (conj
    (AST :number)
    {:value (if (number? value) value ##NaN)}))

(defn add-AST [l r]
  (binary-AST :add l r))

(defn subtract-AST [l r]
  (binary-AST :subtract l r))

(defn multiply-AST [l r]
  (binary-AST :multiply l r))

(defn divide-AST [l r]
  (binary-AST :divide l r))

(defn modulo-AST [l r]
  (binary-AST :modulo l r))

(defn power-AST [base exp]
  (binary-AST :power base exp))

(defn min-AST [l r]
  (binary-AST :min l r))

(defn max-AST [l r]
  (binary-AST :max l r))

(defn kind [ast]
  (if (map? ast) (ast :kind) nil))

(defn canonical
  ([ast]
    (canonical ast 0))
  ([ast depth]
    (str
      (string/join (repeat depth \space))
      (case (kind ast)
        :symbol
          (str "Symbol(" (ast :name) ")\n")
        :number
          (str "Number(" (ast :value) ")\n")
        :add
          (str "Add\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :subtract
          (str "Subtract\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :multiply
          (str "Multiply\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :divide
          (str "Divide\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :modulo
          (str "Modulo\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :power
          (str "Power\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :min
          (str "Min\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))
        :max
          (str "Max\n" (canonical (ast :left) (inc depth)) (canonical (ast :right) (inc depth)))))))
