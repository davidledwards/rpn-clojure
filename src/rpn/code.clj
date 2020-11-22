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
(ns rpn.code
  "Instructions generated from a syntax tree.

  sym <symbol>
  pushsym <symbol>
  push <number>
  add <args>
  sub <args>
  mul <args>
  div <args>
  min <args>
  max <args>
  mod
  pow
  nop"
  (:require [clojure.string :as string]))

(def kinds
  #{:declare-symbol
    :push-symbol
    :push
    :add
    :subtract
    :multiply
    :divide
    :min
    :max
    :modulo
    :power
    :nop})

(defn- code [kind]
  {:kind kind})

(defn- binary-operator [assoc? commut?]
  {:argn 2 :operator :binary :assoc assoc? :commut commut?})

(defn- multiary-operator [argn assoc? commut?]
  {:argn argn :operator :multiary :assoc assoc? :commut commut?})

(defn- canonicalize [^Double n]
  (string/join
    (reverse
      (drop-while #(= % \.)
        (drop-while #(= % \0)
          (reverse (format "%.10f" n)))))))

(defn declare-symbol-code [name]
  (conj
    (code :declare-symbol)
    {:name name}))

(defn push-symbol-code [name]
  (conj
    (code :push-symbol)
    {:name name}))

(defn push-code [value]
  (conj
    (code :push)
    {:value (double value)}))

(defn add-code [argn]
  (conj
    (code :add)
    (multiary-operator argn true true)))

(defn subtract-code [argn]
  (conj
    (code :subtract)
    (multiary-operator argn false false)))

(defn multiply-code [argn]
  (conj
    (code :multiply)
    (multiary-operator argn true true)))

(defn divide-code [argn]
  (conj
    (code :divide)
    (multiary-operator argn false false)))

(defn min-code [argn]
  (conj
    (code :min)
    (multiary-operator argn true true)))

(defn max-code [argn]
  (conj
    (code :max)
    (multiary-operator argn true true)))

(def modulo-code
  (conj
    (code :modulo)
    (binary-operator false false)))

(def power-code
  (conj
    (code :power)
    (binary-operator false false)))

(def nop-code
  (code :nop))

(defn kind [code]
  (if (map? code) (code :kind) nil))

(defn same-kind? [a b]
  (= (kind a) (kind b)))

(def ^:private instruction-names
  {:declare-symbol "sym"
   :push-symbol "pushsym"
   :push "push"
   :add "add"
   :subtract "sub"
   :multiply "mul"
   :divide "div"
   :min "min"
   :max "max"
   :modulo "mod"
   :power "pow"
   :nop "nop"})

(defn instruction [code]
  (if-some [k (kind code)]
    (str (instruction-names k) " "
      (cond
        (contains? #{:declare-symbol
                     :push-symbol} k)
          (code :name)
        (= k :push)
          (canonicalize (code :value))
        (contains? #{:add
                     :subtract
                     :multiply
                     :divide
                     :min
                     :max} k)
          (code :argn)
        (contains? #{:modulo
                     :power
                     :nop} k)
          ""
        :else
          ""))
      nil))

(def ^:private canonical-names
  (reduce #(conj %1 {%2 (subs (str %2) 1)}) {} kinds))

(defn canonical [code]
  (if-some [k (kind code)]
    (str
      "(" (canonical-names k) " "
      (cond
        (contains? #{:declare-symbol
                     :push-symbol} k)
          (code :name)
        (= k :push)
          (canonicalize (code :value))
        (contains? #{:add
                     :subtract
                     :multiply
                     :divide
                     :min
                     :max} k)
          (code :argn)
        (contains? #{:modulo
                     :power
                     :nop} k)
          ""
        :else
          "")
      ")")
    ""))

(defn declare-symbol-code? [code]
  (= (kind code) :declare-symbol))

(defn push-symbol-code? [code]
  (= (kind code) :push-symbol))

(defn push-code? [code]
  (= (kind code) :push))

(defn nop-code? [code]
  (= (kind code) :nop))

(defn operator? [code]
  (and (map? code) (contains? code :operator)))

(defn multiary-operator? [code]
  (and (map? code) (= (code :operator) :multiary)))

(defn associative-operator? [code]
  (and (map? code) (code :assoc)))

(defn commutative-operator? [code]
  (and (map? code) (code :commut)))

(defn- to-double [s]
  (try
    (Double/parseDouble s)
    (catch NumberFormatException e
      nil)))

(defn- to-int [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e
      nil)))

(defn- verify-argn [s floor ctor]
  (when-let [v (to-int s)]
    (if (< v floor)
      nil
      (ctor v))))

(def ^:private patterns
  {#"\s*sym\s+([a-zA-Z]+)\s*"
     #(declare-symbol-code %)
   #"\s*pushsym\s+([a-zA-Z]+)\s*"
     #(push-symbol-code %)
   #"\s*push\s+(-?\d+|-?\d+\.\d+)\s*"
     #(push-code (to-double %))
   #"\s*add\s+(\d+)\s*"
     #(verify-argn % 2 add-code)
   #"\s*sub\s+(\d+)\s*"
     #(verify-argn % 2 subtract-code)
   #"\s*mul\s+(\d+)\s*"
     #(verify-argn % 2 multiply-code)
   #"\s*div\s+(\d+)\s*"
     #(verify-argn % 2 divide-code)
   #"\s*min\s+(\d+)\s*"
     #(verify-argn % 2 min-code)
   #"\s*max\s+(\d+)\s*"
     #(verify-argn % 2 max-code)
   #"\s*mod\s*"
     (fn [_] modulo-code)
   #"\s*pow\s*"
     (fn [_] power-code)
   #"\s*nop\s*"
     (fn [_] nop-code)})

(defn parse [s]
  (some
    (fn [e]
      (when-let [r (re-matches (first e) s)]
        ((second e) (second r))))
    patterns))

(defn symbols [codes]
  (map #(% :name) (filter #(declare-symbol-code? %) codes)))
