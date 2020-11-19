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

(def ^:private canonical-names
  (reduce #(conj %1 {%2 (subs (str %2) 1)}) {} kinds))

(defn canonical [ast]
  (if-some [k (kind ast)]
    (str
      "(" (canonical-names k) " "
      (case k
        :symbol
          (ast :name)
        :number
          (ast :value)
        (str (canonical (ast :left)) " " (canonical (ast :right))))
      ")")
    ""))

(defn- tab [n]
  (string/join (repeat (* n 2) \space)))

(defn typeset
  ([ast]
    (typeset ast 0))
  ([ast depth]
    (if-some [k (kind ast)]
      (str
        (if (> depth 0) "\n" "") (tab depth) "(" (canonical-names k)
        (case k
          :symbol
            (str " " (ast :name))
          :number
            (str " " (ast :value))
          (str
            (typeset (ast :left) (inc depth))
            (typeset (ast :right) (inc depth))))
        ")")
      "")))
