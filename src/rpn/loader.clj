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
(ns rpn.loader
  (:require [rpn.code :as code]))

(defn- scan
  ([in]
    (scan in ""))
  ([in s]
    (let [c (first in)]
      (cond
        (= c \newline)
          [s (rest in)]
        (some? c)
          (recur (rest in) (str s c))
        :else
          [(if (empty? s) nil s) in]))))

(defn loader
  "An instruction loader that transforms a sequence of characters into a sequence of
  instructions."
  [in]
  (lazy-seq
    (let [[s in-rest] (scan in)]
      (if (nil? s)
        nil
        (if-let [c (code/parse s)]
          (cons c (loader in-rest))
          (throw (Exception. (str s ": unrecognized or malformed instruction"))))))))
