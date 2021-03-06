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
(ns rpn.input
  (:require [clojure.java.io :as io]))

(defn input
  "Makes an input stream represented by `in` appear as a lazy sequence of
  characters."
  [in]
  (->>
    (io/reader in :encoding "UTF-8")
    ((fn get-char [r]
      (lazy-seq
        (let [c (.read r)]
          (if (= c -1)
            nil
            (cons (char c) (get-char r)))))))))
