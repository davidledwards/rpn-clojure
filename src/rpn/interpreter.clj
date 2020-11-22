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
(ns rpn.interpreter
  "RPN interpreter."
  (:require [rpn.input :as input])
  (:require [rpn.code :as code])
  (:require [rpn.loader :as loader])
  (:require [rpn.evaluator :as evaluator]))

(defn- bind
  ([args]
    (bind args {}))
  ([args syms]
    (let [[s v] (take 2 args)]
      (cond
        (nil? s)
          syms
        (nil? v)
          (println (str s ": discarding symbol since value is missing"))
        :else
          (bind
            (drop 2 args)
            (try
              (conj syms {s (Double/parseDouble v)})
              (catch NumberFormatException _
                (println (str s " <- " v ": discarding symbol since value is malformed"))
                syms)))))))

(defn -main [& args]
  (try
    (let [in (input/input *in*)
          arg (first args)]
      (cond
        (= arg "-h")
          (do
            (println "usage: rpn [option] [sym val]...")
            (println "  Evaluate instructions from stdin and print result to stdout.")
            (println "  Binds optional sequence of sym/val pairs prior to evaluation.")
            (println "  -s  print symbols"))
        (= arg "-s")
          (doseq [s (code/symbols (loader/loader in))]
            (println s))
        (or (nil? arg) (not= (first arg) \-))
          (let [syms (bind args)]
            (println
              (evaluator/evaluator #(syms %) (loader/loader in))))
        :else
          (println (str arg ": unrecognized option"))))
    (catch Exception e
      (println (.getMessage e)))))
