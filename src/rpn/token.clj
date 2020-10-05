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
  (sorted-set :plus-token
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

(defn- create-token [type lexeme]
  (assert (token-types type) (str type ": unrecognized token type"))
  {:type type :lexeme lexeme})

(defn type-of [token] (token :type))

(defn lexeme-of [token] (token :lexeme))

(defn to-string [token] (str "Token(" (lexeme-of token) ")"))

(def plus-token (create-token :plus-token "+"))
(def minus-token (create-token :minus-token "-"))
(def star-token (create-token :star-token "*"))
(def slash-token (create-token :slash-token "/"))
(def percent-token (create-token :percent-token "%"))
(def caret-token (create-token :caret-token "^"))
(def left-paren-token (create-token :left-paren-token "("))
(def right-paren-token (create-token :right-paren-token ")"))
(def min-token (create-token :min-token "min"))
(def max-token (create-token :max-token "max"))
(def EOS-token (create-token :EOS-token "<EOS>"))

(defn symbol-token [lexeme]
  (create-token :symbol-token lexeme))

(defn number-token [lexeme]
  (create-token :number-token lexeme))

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
  (into (sorted-map)
        (map #(vector (lexeme-of %) %) reserved-tokens)))

(def simple-words
  (into (sorted-set)
        (map first (filter #(== (count %) 1) (keys reserved-words)))))
