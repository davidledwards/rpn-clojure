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
(ns rpn.token)

;;
;; Authoritative set of all recognized token types and their regular expressions.
;;
;; plus-token = +
;; minus-token = -
;; star-token = *
;; slash-token = /
;; percent-token = %
;; caret-token = ^
;; left-paren-token = (
;; right-paren-token = )
;; min-token = min
;; max-token = max
;; symbol-token = [A-Za-z]+
;; number-token = ([0-9]+)|([0-9]+\.[0-9]+)
;; EOS-token = <end of stream>
;;
(def kinds
  #{:plus
    :minus
    :star
    :slash
    :percent
    :caret
    :left-paren
    :right-paren
    :min
    :max
    :symbol
    :number
    :EOS})

(defn- token [kind lexeme]
  {:kind kind :lexeme lexeme})

(def plus-token (token :plus "+"))
(def minus-token (token :minus "-"))
(def star-token (token :star "*"))
(def slash-token (token :slash "/"))
(def percent-token (token :percent "%"))
(def caret-token (token :caret "^"))
(def left-paren-token (token :left-paren "("))
(def right-paren-token (token :right-paren ")"))
(def min-token (token :min "min"))
(def max-token (token :max "max"))
(def EOS-token (token :EOS "<EOS>"))

(defn symbol-token [lexeme]
  (token :symbol lexeme))

(defn number-token [lexeme]
  (token :number lexeme))

(defn kind [token]
  (if (map? token) (token :kind) nil))

(defn same-kind? [a b]
  (= (kind a) (kind b)))

(defn lexeme [token]
  (if (map? token) (token :lexeme) nil))

(defn canonical [token]
  (if-some [l (lexeme token)]
    (str "Token(" l ")")
    ""))

(defn plus-token? [token]
  (= (kind token) :plus))

(defn minus-token? [token]
  (= (kind token) :minus))

(defn star-token? [token]
  (= (kind token) :star))

(defn slash-token? [token]
  (= (kind token) :slash))

(defn percent-token? [token]
  (= (kind token) :percent))

(defn caret-token? [token]
  (= (kind token) :caret))

(defn left-paren-token? [token]
  (= (kind token) :left-paren))

(defn right-paren-token? [token]
  (= (kind token) :right-paren))

(defn min-token? [token]
  (= (kind token) :min))

(defn max-token? [token]
  (= (kind token) :max))

(defn EOS-token? [token]
  (= (kind token) :EOS))

(defn symbol-token? [token]
  (= (kind token) :symbol))

(defn number-token? [token]
  (= (kind token) :number))

(def reserved-tokens
  #{plus-token
    minus-token
    star-token
    slash-token
    percent-token
    caret-token
    left-paren-token
    right-paren-token
    min-token
    max-token})

(def reserved-words
  (into
    (sorted-map)
    (map #(vector (lexeme %) %) reserved-tokens)))

(def simple-words
  (into
    (sorted-set)
    (map first (filter #(== (count %) 1) (keys reserved-words)))))
