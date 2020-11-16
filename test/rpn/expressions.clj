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
(ns rpn.expressions
  (:require [clojure.string :as string])
  (:require [rpn.token :as token]))

(def ^:private whitespace
  [\space \newline \tab \return \formfeed])

(def ^:private letters
  (into [] (map char (concat (range (int \A) (inc (int \Z)))
                             (range (int \a) (inc (int \z)))))))

(defn- generate-space []
  (nth whitespace (rand-int (count whitespace))))

(defn- generate-number []
  (token/number-token (format "%2.2f" (* (rand) 2))))

(defn- generate-symbol []
  (token/symbol-token
    (let [n (count letters)]
      (str
        (nth letters (rand-int n))
        (nth letters (rand-int n))))))

(def ^:private operators
  [token/plus-token
   token/minus-token
   token/star-token
   token/slash-token
   token/percent-token
   token/caret-token
   token/min-token
   token/max-token])

(defn- generate-operator []
  (nth operators (rand-int (count operators))))

(declare generate-expression)

(defn- generate-operand []
  (case (rand-int 3)
    0 [(generate-number)]
    1 [(generate-symbol)]
    2 (conj
        (vec (cons token/left-paren-token (generate-expression)))
        token/right-paren-token)))

(defn- generate-expression []
  (reduce
    (fn [e _] (into [] (concat (conj e (generate-operator)) (generate-operand))))
    (generate-operand) (range (rand-int 4))))

(defn generate
  ([]
    (generate " "))
  ([ws]
    (let [ts (generate-expression)]
      [(string/join ws (map #(token/lexeme %) ts))
        ts])))
