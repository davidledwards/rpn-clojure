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
(ns rpn.compiler
  (:require [rpn.input :as input])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.parser :as parser])
  (:require [rpn.ast :as ast]))

(defn compiler [& args]
  (let [in "ZQ min Tu - ( EE min 0.39 * wi - ( ZU + ( 0.44 + ( ( ( OK ) - 0.51 - 0.21 ) ) ) - ( YV ) / ( ( rP ) min ( ( ( Pz ) - ( Ku * fF ^ ( yO ) % 0.46 ) + 0.89 - RW ) ) max 0.80 ) ) ) "
        lex (lexer/lexer in)
        p (parser/parser lex)]
    (prn in)
    (prn lex)
    (prn p)
    (print (ast/format-AST p))))

(defn -main [& args]
  (try
    (compiler args)
    (catch Exception e
      (println e))))
