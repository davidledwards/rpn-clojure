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
  "RPN compiler."
  (:require [rpn.token :as token])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.parser :as parser])
  (:require [rpn.ast :as ast])
  (:require [rpn.generator :as gen])
  (:require [rpn.optimizer :as opt])
  (:require [rpn.code :as code])
  (:require [rpn.input :as input]))

(defn -main [& args]
  (try
    (let [in (input/input *in*)
          arg (first args)]
      (cond
        (= arg "-h")
          (do
            (println "usage: rpnc [options]")
            (println "  Compile expression from stdin and emit instructions to stdout.")
            (println "  -t tokenize only")
            (println "  -p parse only")
            (println "  -o optimize"))
        (= arg "-t")
          (doseq [t (lexer/lexer in)]
            (println (token/canonical t)))
        (= arg "-p")
          (println (ast/typeset (parser/parser (lexer/lexer in))))
        (= arg "-o")
          (doseq [c (opt/optimizer (gen/generator (parser/parser (lexer/lexer in))))]
            (println (code/instruction c)))
        (nil? arg)
          (doseq [c (gen/generator (parser/parser (lexer/lexer in)))]
            (println (code/instruction c)))
        :else
          (println (str arg ": unrecognized option"))))
    (catch Exception e
      (println (.getMessage e)))))
