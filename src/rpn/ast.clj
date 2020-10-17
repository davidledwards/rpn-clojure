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

(def AST-types
  (sorted-set :symbol-AST
              :number-AST
              :add-AST
              :subtract-AST
              :multiply-AST
              :divide-AST
              :modulo-AST
              :power-AST
              :min-AST
              :max-AST))

(defn type-of [ast]
  (if (map? ast) (ast :type) nil))

(defn- create-AST [type]
  {:type type})

(defn- create-binary-AST [type l r]
  (assoc (create-AST type) :left l :right r))

(defn symbol-AST [name]
  (assoc
    (create-AST :symbol-AST)
    :name (if (string? name) name (str name))))

(defn number-AST [value]
  (assoc
    (create-AST :number-AST)
    :value (if (number? value) value ##NaN)))

(defn add-AST [l r]
  (create-binary-AST :add-AST l r))

(defn subtract-AST [l r]
  (create-binary-AST :subtract-AST l r))

(defn multiply-AST [l r]
  (create-binary-AST :multiply-AST l r))

(defn divide-AST [l r]
  (create-binary-AST :divide-AST l r))

(defn modulo-AST [l r]
  (create-binary-AST :modulo-AST l r))

(defn power-AST [base exp]
  (create-binary-AST :power-AST base exp))

(defn min-AST [l r]
  (create-binary-AST :min-AST l r))

(defn max-AST [l r]
  (create-binary-AST :max-AST l r))

(defn format-AST
  ([ast]
    (format-AST ast 0))
  ([ast depth]
    (str
      (string/join (repeat depth \space))
      (case (type-of ast)
        :symbol-AST
          (str "Symbol(" (ast :name) ")\n")
        :number-AST
          (str "Number(" (ast :value) ")\n")
        :add-AST
          (str "Add\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :subtract-AST
          (str "Subtract\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :multiply-AST
          (str "Multiply\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :divide-AST
          (str "Divide\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :modulo-AST
          (str "Modulo\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :power-AST
          (str "Power\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :min-AST
          (str "Min\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))
        :max-AST
          (str "Max\n" (format-AST (ast :left) (inc depth)) (format-AST (ast :right) (inc depth)))))))
