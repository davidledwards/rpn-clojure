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
(ns rpn.tools
  (:require [clojure.string :as string])
  (:require [rpn.expressions :as expr])
  (:require [rpn.token :as token])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.parser :as parser])
  (:require [rpn.ast :as ast]))

(defn- name-hash [name]
  (/ (reduce #(+ %1 (/ (int %2) 100.0)) 0.0 name) 10.))

(defn- tab [n]
  (string/join (repeat (* n 2) \space)))

(defn- parser-tests [n]
  (do
    (print (str "(list"))
    (doseq [i (range n)]
      (let [[e _] (expr/generate)]
        (print (str "\n" (tab 1) "[\"" e "\""))
        (let [ast (parser/parser (lexer/lexer e))]
          ((fn emit
            ([ast-fn depth l r]
              (print ast-fn)
              (emit l (inc depth))
              (emit r (inc depth)))
            ([ast depth]
              (print (str "\n" (tab depth) "("))
              (case (ast/kind ast)
                :symbol
                  (print (str "rpn.ast/symbol-AST \"" (ast :name) "\""))
                :number
                  (print (str "rpn.ast/number-AST " (ast :value)))
                :add
                  (emit "rpn.ast/add-AST" depth (ast :left) (ast :right))
                :subtract
                  (emit "rpn.ast/subtract-AST" depth (ast :left) (ast :right))
                :multiply
                  (emit "rpn.ast/multiply-AST" depth (ast :left) (ast :right))
                :divide
                  (emit "rpn.ast/divide-AST" depth (ast :left) (ast :right))
                :modulo
                  (emit "rpn.ast/modulo-AST" depth (ast :left) (ast :right))
                :power
                  (emit "rpn.ast/power-AST" depth (ast :left) (ast :right))
                :min
                  (emit "rpn.ast/min-AST" depth (ast :left) (ast :right))
                :max
                  (emit "rpn.ast/max-AST" depth (ast :left) (ast :right)))
              (print ")")))
            ast 2))
          (print "]")))
    (println ")")))

(defn gen-parser-tests [arg-map]
  (parser-tests
    (or (and (map? arg-map) (arg-map :count)) 100)))

(defn generator-tests []
  nil)

(defn optimizer-tests []
  nil)