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
(ns rpn.lexer-test
  (:require [clojure.test :as test])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.token :as token])
  (:require [rpn.expressions :as expr]))

(test/deftest valid-tokens
  (test/testing "valid tokens in randomized expressions"
    (dotimes [_ 100]
      (let [[e ts] (expr/generate)
            tokens (lexer/lexer e)]
        (test/is (= (count tokens) (count ts)))
        (for [[x y] (map vector tokens ts)]
          (test/is (= (token/lexeme x) (token/lexeme y))))))))

(test/deftest malformed-numbers
  (test/testing "malformed numbers"
    (doseq [e ["."
               ".1"
               "1."
               "1.2."]]
      (test/is (thrown? Exception
        (doall (lexer/lexer e)))))))
