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
  (:require [clojure.string :as string]))

;;
;; Represents a bytecode instruction generated from a syntax tree.
;;
;; Recognized instructions:
;; sym <symbol>
;; pushsym <symbol>
;; push <number>
;; add <args>
;; sub <args>
;; mul <args>
;; div <args>
;; min <args>
;; max <args>
;; mod
;; pow
;; nop
;;
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

(defn- code [kind ins canon]
  {:kind kind :ins ins :canon canon})

(defn- binary-operator [assoc? commut?]
  {:argn 2 :operator :binary :assoc assoc? :commut commut?})

(defn- multiary-operator [argn assoc? commut?]
  {:argn argn :operator :multiary :assoc assoc? :commut commut?})

(defn- to-canonical [^Double n]
  (string/join
    (reverse
      (drop-while #(= % \.)
        (drop-while #(= % \0)
          (reverse (format "%.10f" n)))))))

(defn declare-symbol-code [name]
  (conj
    (code :declare-symbol
      (str "sym " name)
      (str "DeclareSymbol(" name ")"))
    {:name name}))

(defn push-symbol-code [name]
  (code :push-symbol
    (str "pushsym " name)
    (str "PushSymbol(" name ")")))

(defn push-code [value]
  (let [v (double value)]
    (conj
      (code :push
        (str "push " (to-canonical v))
        (str "Push(" v ")"))
      {:value v})))

(defn add-code [argn]
  (conj
    (code :add
      (str "add " argn)
      (str "Add(" argn ")"))
    (multiary-operator argn true true)))

(defn subtract-code [argn]
  (conj
    (code :subtract
      (str "sub " argn)
      (str "Subtract(" argn ")"))
    (multiary-operator argn false false)))

(defn multiply-code [argn]
  (conj
    (code :multiply
      (str "mul " argn)
      (str "Multiply(" argn ")"))
    (multiary-operator argn true true)))

(defn divide-code [argn]
  (conj
    (code :divide
      (str "div " argn)
      (str "Divide(" argn ")"))
    (multiary-operator argn false false)))

(defn min-code [argn]
  (conj
    (code :min
      (str "min " argn)
      (str "Min(" argn ")"))
    (multiary-operator argn true true)))

(defn max-code [argn]
  (conj
    (code :max
      (str "max " argn)
      (str "Max(" argn ")"))
    (multiary-operator argn true true)))

(def modulo-code
  (conj
    (code :modulo "mod" "Modulo()")
    (binary-operator false false)))

(def power-code
  (conj
    (code :power "pow" "Power()")
    (binary-operator false false)))

(def nop-code
  (code :nop "nop" "Nop()"))

(defn kind [code]
  (if (map? code) (code :kind) nil))

(defn same-kind? [a b]
  (= (kind a) (kind b)))

(defn instruction [code]
  (if (map? code) (code :ins) nil))

(defn canonical [code]
  (if (map? code) (code :canon) ""))

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
