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
(ns rpn.lexer
  "Lexical analyzer."
  (:require [clojure.string :as string])
  (:require [rpn.token :as token]))

(def ^:private digits
  (set (map char (range (int \0) (inc (int \9))))))

(def ^:private letters
  (set (map char (concat (range (int \A) (inc (int \Z))) (range (int \a) (inc (int \z)))))))

(def ^:private whitespace
  #{\space \newline \tab \return \formfeed})

(defn- read-number [in lexeme]
  (let [c (first in)]
    (cond
      (= c \.)
        (if (string/includes? lexeme (str c))
          (throw (Exception. (str lexeme ": malformed number")))
          (recur (rest in) (str lexeme \.)))
      (digits c)
        (recur (rest in) (str lexeme c))
      :else
        (if (= (last lexeme) \.)
          (throw (Exception. (str lexeme ": malformed number")))
          [(token/number-token lexeme) in]))))

(defn- read-symbol [in lexeme]
  (let [c (first in)]
    (cond
      (letters c)
        (recur (rest in) (str lexeme c))
      :else
        [(token/reserved-words lexeme (token/symbol-token lexeme)) in])))

(defn- tokenize [in]
  (let [c (first in)]
    (cond
      (whitespace c)
        (recur (rest in))
      (token/simple-words c)
        [(token/reserved-words (str c)) (rest in)]
      (digits c)
        (read-number (rest in) (str c))
      (letters c)
        (read-symbol (rest in) (str c))
      (nil? c)
        [token/EOS-token in]
      :else
        (throw (Exception. (str c ": unrecognized character"))))))

(defn lexer
  "A lexical analyzer that transforms a sequence of characters from `in` into a
  lazy sequence of tokens.

  Tokens must either be delimited by one or more whitespace characters, or be clearly
  distinguishable from each other if not separated by whitespace."
  [in]
  (lazy-seq
    (let [[t in-rest] (tokenize in)]
      (if (token/EOS-token? t)
        nil
        (cons t (lexer in-rest))))))
