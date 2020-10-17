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
(def token-types
  (sorted-set
    :plus-token
    :minus-token
    :star-token
    :slash-token
    :percent-token
    :caret-token
    :left-paren-token
    :right-paren-token
    :min-token
    :max-token
    :symbol-token
    :number-token
    :EOS-token))

(defn type-of [token]
  (if (map? token) (token :type) nil))

(defn lexeme-of [token]
  (if (map? token) (token :lexeme) nil))

(defn to-string [token]
  (if-some [l (lexeme-of token)]
    (str "Token(" l ")")
    ""))

(defn- create-token [type lexeme]
  {:type type :lexeme lexeme})

(def plus-token (create-token :plus-token "+"))

(defn plus-token? [token]
  (= (type-of token) :plus-token))

(def minus-token (create-token :minus-token "-"))

(defn minus-token? [token]
  (= (type-of token) :minus-token))

(def star-token (create-token :star-token "*"))

(defn star-token? [token]
  (= (type-of token) :star-token))

(def slash-token (create-token :slash-token "/"))

(defn slash-token? [token]
  (= (type-of token) :slash-token))

(def percent-token (create-token :percent-token "%"))

(defn percent-token? [token]
  (= (type-of token) :percent-token))

(def caret-token (create-token :caret-token "^"))

(defn caret-token? [token]
  (= (type-of token) :caret-token))

(def left-paren-token (create-token :left-paren-token "("))

(defn left-paren-token? [token]
  (= (type-of token) :left-paren-token))

(def right-paren-token (create-token :right-paren-token ")"))

(defn right-paren-token? [token]
  (= (type-of token) :right-paren-token))

(def min-token (create-token :min-token "min"))

(defn min-token? [token]
  (= (type-of token) :min-token))

(def max-token (create-token :max-token "max"))

(defn max-token? [token]
  (= (type-of token) :max-token))

(def EOS-token (create-token :EOS-token "<EOS>"))

(defn EOS-token? [token]
  (= (type-of token) :EOS-token))

(defn symbol-token [lexeme]
  (create-token :symbol-token lexeme))

(defn symbol-token? [token]
  (= (type-of token) :symbol-token))

(defn number-token [lexeme]
  (create-token :number-token lexeme))

(defn number-token? [token]
  (= (type-of token) :number-token))

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
    (map #(vector (lexeme-of %) %) reserved-tokens)))

(def simple-words
  (into
    (sorted-set)
    (map first (filter #(== (count %) 1) (keys reserved-words)))))
