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
(ns rpn.parser
  (:require [rpn.token :as token])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.ast :as ast]))

;;
;; A recursive-descent parser that transforms a sequence of tokens into a syntax tree.
;;
;; Grammar:
;;
;; p0 ::= <p2> <p1>
;; p1 ::= '+' <p2> <p1>
;;    ::= '-' <p2> <p1>
;;    ::= e
;; p2 ::= <p4> <p3>
;; p3 ::= '*' <p4> <p3>
;;    ::= '/' <p4> <p3>
;;    ::= '%' <p4> <p3>
;;    ::= '^' <p4> <p3>
;;    ::= e
;; p4 ::= <p6> <p5>
;; p5 ::= 'min' <p6> <p5>
;;    ::= 'max' <p6> <p5>
;;    ::= e
;; p6 ::= '(' <p0> ')'
;;    ::= <symbol>
;;    ::= <number>
;;

(declare p0 p1 p2 p3 p4 p5 p6)

(defn- match [in token]
  (let [t (first in)]
    (if (= (token/type-of t) (token/type-of token))
      (rest in)
      (let [lexeme (token/lexeme-of (or t token/EOS-token))]
        (throw (Exception. (str lexeme ": expecting '" (token/lexeme-of token) "'")))))))

;;
;; p6 ::= '(' <p0> ')'
;;    ::= <symbol>
;;    ::= <number>
;;
(defn p6 [in]
  (let [t (first in)]
    (case (token/type-of t)
      :left-paren-token
        (let [[ast in-rest] (p0 (rest in))]
          [ast (match in-rest token/right-paren-token)])
      :symbol-token
        [(ast/symbol-AST (token/lexeme-of t)) (rest in)]
      :number-token
        [(ast/number-AST (Double/parseDouble (token/lexeme-of t))) (rest in)]
      (let [lexeme (token/lexeme-of (or t token/EOS-token))]
        (throw (Exception. (str lexeme ": expecting '{', <symbol> or <number>")))))))

;;
;; p5 ::= 'min' <p6> <p5>
;;    ::= 'max' <p6> <p5>
;;    ::= e
;;
(defn p5 [l in]
  (let [t (first in)]
    (case (token/type-of t)
      :min-token
        (let [[r in-rest] (p6 (rest in))]
          (p5 (ast/min-AST l r) in-rest))
      :max-token
        (let [[r in-rest] (p6 (rest in))]
          (p5 (ast/max-AST l r) in-rest))
      [l in])))

;;
;; p4 ::= <p6> <p5>
;;
(defn p4 [in]
  (let [[l in-rest] (p6 in)]
    (p5 l in-rest)))

;;
;; p3 ::= '*' <p4> <p3>
;;    ::= '/' <p4> <p3>
;;    ::= '%' <p4> <p3>
;;    ::= '^' <p4> <p3>
;;    ::= e
;;
(defn p3 [l in]
  (let [t (first in)]
    (case (token/type-of t)
      :star-token
        (let [[r in-rest] (p4 (rest in))]
          (p3 (ast/multiply-AST l r) in-rest))
      :slash-token
        (let [[r in-rest] (p4 (rest in))]
          (p3 (ast/divide-AST l r) in-rest))
      :percent-token
        (let [[r in-rest] (p4 (rest in))]
          (p3 (ast/modulo-AST l r) in-rest))
      :caret-token
        (let [[r in-rest] (p4 (rest in))]
          (p3 (ast/power-AST l r) in-rest))
      [l in])))

;;
;; p2 ::= <p4> <p3>
;;
(defn p2 [in]
  (let [[l in-rest] (p4 in)]
    (p3 l in-rest)))

;;
;; p1 ::= '+' <p2> <p1>
;;    ::= '-' <p2> <p1>
;;    ::= e
;;
(defn p1 [l in]
  (let [t (first in)]
    (case (token/type-of t)
      :plus-token
        (let [[r in-rest] (p2 (rest in))]
          (p1 (ast/add-AST l r) in-rest))
      :minus-token
        (let [[r in-rest] (p2 (rest in))]
          (p1 (ast/subtract-AST l r) in-rest))
      [l in])))

;;
;; p0 ::= <p2> <p1>
;;
(defn p0 [in]
  (let [[l in-rest] (p2 in)]
    (p1 l in-rest)))

(defn parser [in]
  (let [[ast in-rest] (p0 in)
        t (first in-rest)]
    (if (nil? t)
      ast
      (throw (Exception. (str (token/lexeme-of t) ": expecting " (token/lexeme-of token/EOS-token)))))))