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
  (:require [rpn.ast :as ast])
  (:require [rpn.generator :as gen])
  (:require [rpn.code :as code])
  (:require [rpn.evaluator :as evaluator]))

(defn name-hash [name]
  (/ (reduce #(+ %1 (/ (int %2) 100.0)) 0.0 name) 10.0))

(defn- tab [n]
  (string/join (repeat (* n 2) \space)))

(defn- parser-tests [n]
  (print "(list")
  (doseq [_ (range n)]
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
  (println ")"))

(defn- generator-tests [n]
  (print "(list")
  (doseq [_ (range n)]
    (let [[e _] (expr/generate)]
      (println (str "\n" (tab 1) "[\"" e "\""))
      (print (str (tab 2) "(list"))
      (doseq [c (gen/generator (parser/parser (lexer/lexer e)))]
        (print
          (str "\n" (tab 3)
            (case (code/kind c)
              :declare-symbol
                (str "(rpn.code/declare-symbol-code \"" (c :name) "\")")
              :push-symbol
                (str "(rpn.code/push-symbol-code \"" (c :name) "\")")
              :push
                (str "(rpn.code/push-code " (c :value) ")")
              :add
                (str "(rpn.code/add-code " (c :argn) ")")
              :subtract
                (str "(rpn.code/subtract-code " (c :argn) ")")
              :multiply
                (str "(rpn.code/multiply-code " (c :argn) ")")
              :divide
                (str "(rpn.code/divide-code " (c :argn) ")")
              :min
                (str "(rpn.code/min-code " (c :argn) ")")
              :max
                (str "(rpn.code/max-code " (c :argn) ")")
              :modulo
                "rpn.code/modulo-code"
              :power
                "rpn.code/power-code"))))
      (print ")]")))
  (println ")"))

(defn- optimizer-tests [n]
  (print "(list")
  (doseq [_ (range n)]
    (let [[e _] (expr/generate)
          cs (gen/generator (parser/parser (lexer/lexer e)))
          r (evaluator/evaluator #(name-hash %) cs)]
      (if-not (Double/isNaN r)
        (do
          (print (str "\n" (tab 1) "["))
          (println r)
          (print (str (tab 2) "(list"))
          (doseq [c cs]
            (print
              (str "\n" (tab 3)
                (case (code/kind c)
                  :declare-symbol
                    (str "(rpn.code/declare-symbol-code \"" (c :name) "\")")
                  :push-symbol
                    (str "(rpn.code/push-symbol-code \"" (c :name) "\")")
                  :push
                    (str "(rpn.code/push-code " (c :value) ")")
                  :add
                    (str "(rpn.code/add-code " (c :argn) ")")
                  :subtract
                    (str "(rpn.code/subtract-code " (c :argn) ")")
                  :multiply
                    (str "(rpn.code/multiply-code " (c :argn) ")")
                  :divide
                    (str "(rpn.code/divide-code " (c :argn) ")")
                  :min
                    (str "(rpn.code/min-code " (c :argn) ")")
                  :max
                    (str "(rpn.code/max-code " (c :argn) ")")
                  :modulo
                    "rpn.code/modulo-code"
                  :power
                    "rpn.code/power-code"))))
          (print ")]")))))
  (println ")"))

(defn- get-count [arg]
  (if arg (Integer/parseInt arg) 100))

(defn -main [& args]
  (try
    (case (first args)
      "-p"
        (parser-tests (get-count (second args)))
      "-g"
        (generator-tests (get-count (second args)))
      "-o"
        (optimizer-tests (get-count (second args)))
      (do
        (println "Generate random expressions for inclusion in unit tests.")
        (println "  -p COUNT  generate parser tests")
        (println "  -g COUNT  generate generator tests")
        (println "  -o COUNT  generate optimizer tests")))
    (catch Exception e
      (println e))))
