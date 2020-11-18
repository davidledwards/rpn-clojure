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
(ns rpn.parser-test
  (:require [clojure.test :as test])
  (:require [rpn.expressions :as expr])
  (:require [rpn.tools :as tools])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.parser :as parser]))

(declare parser-tests)

(test/deftest valid-randomized-expressions
  (doseq [[e ast] parser-tests]
    (test/is (= (parser/parser (lexer/lexer e)) ast))))

(def ^:private parser-tests
  (list
    ["( 0.39 * ( 0.57 ) min 0.13 ) max 1.78 min 1.64 max ( nT ^ ( 0.47 % ( ( jQ max qh ^ PK % 1.01 ) ^ 1.24 max 0.50 max ( ( VU ^ 0.57 ) + qx ) ) - ( 1.71 ) max gx ) )"
      (rpn.ast/max-AST
        (rpn.ast/min-AST
          (rpn.ast/max-AST
            (rpn.ast/multiply-AST
              (rpn.ast/number-AST 0.39)
              (rpn.ast/min-AST
                (rpn.ast/number-AST 0.57)
                (rpn.ast/number-AST 0.13)))
            (rpn.ast/number-AST 1.78))
          (rpn.ast/number-AST 1.64))
        (rpn.ast/power-AST
          (rpn.ast/symbol-AST "nT")
          (rpn.ast/subtract-AST
            (rpn.ast/modulo-AST
              (rpn.ast/number-AST 0.47)
              (rpn.ast/power-AST
                (rpn.ast/modulo-AST
                  (rpn.ast/power-AST
                    (rpn.ast/max-AST
                      (rpn.ast/symbol-AST "jQ")
                      (rpn.ast/symbol-AST "qh"))
                    (rpn.ast/symbol-AST "PK"))
                  (rpn.ast/number-AST 1.01))
                (rpn.ast/max-AST
                  (rpn.ast/max-AST
                    (rpn.ast/number-AST 1.24)
                    (rpn.ast/number-AST 0.5))
                  (rpn.ast/add-AST
                    (rpn.ast/power-AST
                      (rpn.ast/symbol-AST "VU")
                      (rpn.ast/number-AST 0.57))
                    (rpn.ast/symbol-AST "qx")))))
            (rpn.ast/max-AST
              (rpn.ast/number-AST 1.71)
              (rpn.ast/symbol-AST "gx")))))]
    ["0.23 / XT + 0.08"
      (rpn.ast/add-AST
        (rpn.ast/divide-AST
          (rpn.ast/number-AST 0.23)
          (rpn.ast/symbol-AST "XT"))
        (rpn.ast/number-AST 0.08))]
    ["TM"
      (rpn.ast/symbol-AST "TM")]
    ["DM * ( RL ) / ( ( ( 1.30 min JJ * ( 1.02 max ( 0.61 * GQ / 0.64 ) max ( ( 1.45 ) * AB ^ ( ( ( 1.75 + 1.79 * ( tO + Lp max us min 1.67 ) ) * ( 0.10 min 0.32 ) min ( 0.01 ) ) * ( ( 1.73 ) ) - 0.16 ) * 0.85 ) ) ) % ( gY max oQ / ( 0.96 % 1.02 - 0.17 ) ) min gS * 1.05 ) ^ wW ) max ( ea min ( ( ( ( ( Ps ) ) / xr ) ) max 1.37 max 0.31 ) )"
      (rpn.ast/divide-AST
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "DM")
          (rpn.ast/symbol-AST "RL"))
        (rpn.ast/max-AST
          (rpn.ast/power-AST
            (rpn.ast/multiply-AST
              (rpn.ast/modulo-AST
                (rpn.ast/multiply-AST
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 1.3)
                    (rpn.ast/symbol-AST "JJ"))
                  (rpn.ast/max-AST
                    (rpn.ast/max-AST
                      (rpn.ast/number-AST 1.02)
                      (rpn.ast/divide-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/number-AST 0.61)
                          (rpn.ast/symbol-AST "GQ"))
                        (rpn.ast/number-AST 0.64)))
                    (rpn.ast/multiply-AST
                      (rpn.ast/power-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/number-AST 1.45)
                          (rpn.ast/symbol-AST "AB"))
                        (rpn.ast/subtract-AST
                          (rpn.ast/multiply-AST
                            (rpn.ast/multiply-AST
                              (rpn.ast/add-AST
                                (rpn.ast/number-AST 1.75)
                                (rpn.ast/multiply-AST
                                  (rpn.ast/number-AST 1.79)
                                  (rpn.ast/add-AST
                                    (rpn.ast/symbol-AST "tO")
                                    (rpn.ast/min-AST
                                      (rpn.ast/max-AST
                                        (rpn.ast/symbol-AST "Lp")
                                        (rpn.ast/symbol-AST "us"))
                                      (rpn.ast/number-AST 1.67)))))
                              (rpn.ast/min-AST
                                (rpn.ast/min-AST
                                  (rpn.ast/number-AST 0.1)
                                  (rpn.ast/number-AST 0.32))
                                (rpn.ast/number-AST 0.01)))
                            (rpn.ast/number-AST 1.73))
                          (rpn.ast/number-AST 0.16)))
                      (rpn.ast/number-AST 0.85))))
                (rpn.ast/min-AST
                  (rpn.ast/divide-AST
                    (rpn.ast/max-AST
                      (rpn.ast/symbol-AST "gY")
                      (rpn.ast/symbol-AST "oQ"))
                    (rpn.ast/subtract-AST
                      (rpn.ast/modulo-AST
                        (rpn.ast/number-AST 0.96)
                        (rpn.ast/number-AST 1.02))
                      (rpn.ast/number-AST 0.17)))
                  (rpn.ast/symbol-AST "gS")))
              (rpn.ast/number-AST 1.05))
            (rpn.ast/symbol-AST "wW"))
          (rpn.ast/min-AST
            (rpn.ast/symbol-AST "ea")
            (rpn.ast/max-AST
              (rpn.ast/max-AST
                (rpn.ast/divide-AST
                  (rpn.ast/symbol-AST "Ps")
                  (rpn.ast/symbol-AST "xr"))
                (rpn.ast/number-AST 1.37))
              (rpn.ast/number-AST 0.31)))))]
    ["0.56 - ( ( ( ( ( ( ( 1.66 ) min 1.58 ) * ( 1.54 + ( 1.63 * ( Qu % qD % 0.34 ) ^ 1.48 + oY ) ) * ( 1.64 ^ 1.95 ^ ( 0.19 ^ ( lk max 1.48 ) ) ) ) max Ue + 0.46 ^ 1.67 ) % FT / Ni min ( Ls / zq + ( 0.07 / ( 0.17 ) min ( 0.25 ) ) ) ) max lY ) min ( Vv max 1.23 ) ) % rr"
      (rpn.ast/subtract-AST
        (rpn.ast/number-AST 0.56)
        (rpn.ast/modulo-AST
          (rpn.ast/min-AST
            (rpn.ast/max-AST
              (rpn.ast/divide-AST
                (rpn.ast/modulo-AST
                  (rpn.ast/add-AST
                    (rpn.ast/max-AST
                      (rpn.ast/multiply-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/min-AST
                            (rpn.ast/number-AST 1.66)
                            (rpn.ast/number-AST 1.58))
                          (rpn.ast/add-AST
                            (rpn.ast/number-AST 1.54)
                            (rpn.ast/add-AST
                              (rpn.ast/power-AST
                                (rpn.ast/multiply-AST
                                  (rpn.ast/number-AST 1.63)
                                  (rpn.ast/modulo-AST
                                    (rpn.ast/modulo-AST
                                      (rpn.ast/symbol-AST "Qu")
                                      (rpn.ast/symbol-AST "qD"))
                                    (rpn.ast/number-AST 0.34)))
                                (rpn.ast/number-AST 1.48))
                              (rpn.ast/symbol-AST "oY"))))
                        (rpn.ast/power-AST
                          (rpn.ast/power-AST
                            (rpn.ast/number-AST 1.64)
                            (rpn.ast/number-AST 1.95))
                          (rpn.ast/power-AST
                            (rpn.ast/number-AST 0.19)
                            (rpn.ast/max-AST
                              (rpn.ast/symbol-AST "lk")
                              (rpn.ast/number-AST 1.48)))))
                      (rpn.ast/symbol-AST "Ue"))
                    (rpn.ast/power-AST
                      (rpn.ast/number-AST 0.46)
                      (rpn.ast/number-AST 1.67)))
                  (rpn.ast/symbol-AST "FT"))
                (rpn.ast/min-AST
                  (rpn.ast/symbol-AST "Ni")
                  (rpn.ast/add-AST
                    (rpn.ast/divide-AST
                      (rpn.ast/symbol-AST "Ls")
                      (rpn.ast/symbol-AST "zq"))
                    (rpn.ast/divide-AST
                      (rpn.ast/number-AST 0.07)
                      (rpn.ast/min-AST
                        (rpn.ast/number-AST 0.17)
                        (rpn.ast/number-AST 0.25))))))
              (rpn.ast/symbol-AST "lY"))
            (rpn.ast/max-AST
              (rpn.ast/symbol-AST "Vv")
              (rpn.ast/number-AST 1.23)))
          (rpn.ast/symbol-AST "rr")))]
    ["0.78 ^ ( xm max 0.85 + mJ )"
      (rpn.ast/power-AST
        (rpn.ast/number-AST 0.78)
        (rpn.ast/add-AST
          (rpn.ast/max-AST
            (rpn.ast/symbol-AST "xm")
            (rpn.ast/number-AST 0.85))
          (rpn.ast/symbol-AST "mJ")))]
    ["xD * Iu"
      (rpn.ast/multiply-AST
        (rpn.ast/symbol-AST "xD")
        (rpn.ast/symbol-AST "Iu"))]
    ["dG * Xx / di"
      (rpn.ast/divide-AST
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "dG")
          (rpn.ast/symbol-AST "Xx"))
        (rpn.ast/symbol-AST "di"))]
    ["Ur max TC - 0.10"
      (rpn.ast/subtract-AST
        (rpn.ast/max-AST
          (rpn.ast/symbol-AST "Ur")
          (rpn.ast/symbol-AST "TC"))
        (rpn.ast/number-AST 0.1))]
    ["( 1.30 ^ ( ( 1.01 min 1.95 min 1.15 ) * lp + kT ) ) % ( ( ( 0.91 ) ) - hv max ( Wu ) * 0.23 ) * 0.18"
      (rpn.ast/multiply-AST
        (rpn.ast/modulo-AST
          (rpn.ast/power-AST
            (rpn.ast/number-AST 1.3)
            (rpn.ast/add-AST
              (rpn.ast/multiply-AST
                (rpn.ast/min-AST
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 1.01)
                    (rpn.ast/number-AST 1.95))
                  (rpn.ast/number-AST 1.15))
                (rpn.ast/symbol-AST "lp"))
              (rpn.ast/symbol-AST "kT")))
          (rpn.ast/subtract-AST
            (rpn.ast/number-AST 0.91)
            (rpn.ast/multiply-AST
              (rpn.ast/max-AST
                (rpn.ast/symbol-AST "hv")
                (rpn.ast/symbol-AST "Wu"))
              (rpn.ast/number-AST 0.23))))
        (rpn.ast/number-AST 0.18))]
    ["YN - dl"
      (rpn.ast/subtract-AST
        (rpn.ast/symbol-AST "YN")
        (rpn.ast/symbol-AST "dl"))]
    ["( Uv )"
      (rpn.ast/symbol-AST "Uv")]
    ["1.20 - ea min 1.61"
      (rpn.ast/subtract-AST
        (rpn.ast/number-AST 1.2)
        (rpn.ast/min-AST
          (rpn.ast/symbol-AST "ea")
          (rpn.ast/number-AST 1.61)))]
    ["xN * 0.06"
      (rpn.ast/multiply-AST
        (rpn.ast/symbol-AST "xN")
        (rpn.ast/number-AST 0.06))]
    ["LE ^ tg max nZ ^ XY"
      (rpn.ast/power-AST
        (rpn.ast/power-AST
          (rpn.ast/symbol-AST "LE")
          (rpn.ast/max-AST
            (rpn.ast/symbol-AST "tg")
            (rpn.ast/symbol-AST "nZ")))
        (rpn.ast/symbol-AST "XY"))]
    ["( ( ( ( Dx ^ 0.48 ) % ( ( ZE ^ Fq - 1.64 % ( ya / Ea % 1.67 + ( 1.73 % fN ) ) ) ) min 0.26 ) max ( JI - ( ( Dq max ( 1.68 max UZ * HZ * 0.08 ) % 1.13 - 1.67 ) ) ) ) min ( 0.01 ) / aD * 0.61 )"
      (rpn.ast/multiply-AST
        (rpn.ast/divide-AST
          (rpn.ast/min-AST
            (rpn.ast/max-AST
              (rpn.ast/modulo-AST
                (rpn.ast/power-AST
                  (rpn.ast/symbol-AST "Dx")
                  (rpn.ast/number-AST 0.48))
                (rpn.ast/min-AST
                  (rpn.ast/subtract-AST
                    (rpn.ast/power-AST
                      (rpn.ast/symbol-AST "ZE")
                      (rpn.ast/symbol-AST "Fq"))
                    (rpn.ast/modulo-AST
                      (rpn.ast/number-AST 1.64)
                      (rpn.ast/add-AST
                        (rpn.ast/modulo-AST
                          (rpn.ast/divide-AST
                            (rpn.ast/symbol-AST "ya")
                            (rpn.ast/symbol-AST "Ea"))
                          (rpn.ast/number-AST 1.67))
                        (rpn.ast/modulo-AST
                          (rpn.ast/number-AST 1.73)
                          (rpn.ast/symbol-AST "fN")))))
                  (rpn.ast/number-AST 0.26)))
              (rpn.ast/subtract-AST
                (rpn.ast/symbol-AST "JI")
                (rpn.ast/subtract-AST
                  (rpn.ast/modulo-AST
                    (rpn.ast/max-AST
                      (rpn.ast/symbol-AST "Dq")
                      (rpn.ast/multiply-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/max-AST
                            (rpn.ast/number-AST 1.68)
                            (rpn.ast/symbol-AST "UZ"))
                          (rpn.ast/symbol-AST "HZ"))
                        (rpn.ast/number-AST 0.08)))
                    (rpn.ast/number-AST 1.13))
                  (rpn.ast/number-AST 1.67))))
            (rpn.ast/number-AST 0.01))
          (rpn.ast/symbol-AST "aD"))
        (rpn.ast/number-AST 0.61))]
    ["( ( ( ( ( ( ( 1.25 + 1.18 - ( ( ( 0.60 ) * ( ( 1.23 min yT * Xf ) / iA ) ) * JG + XB max Ma ) ) min ( 1.42 ) max ( ( 0.05 - ( Bb / so ) ) ) ) ) * gk ) ^ 0.09 max mq ) min gj ) % ( EA * uk min 0.77 ^ CY ) min ( ( Fi min 0.62 ) * 0.41 ) ^ 0.62 )"
      (rpn.ast/power-AST
        (rpn.ast/modulo-AST
          (rpn.ast/min-AST
            (rpn.ast/power-AST
              (rpn.ast/multiply-AST
                (rpn.ast/max-AST
                  (rpn.ast/min-AST
                    (rpn.ast/subtract-AST
                      (rpn.ast/add-AST
                        (rpn.ast/number-AST 1.25)
                        (rpn.ast/number-AST 1.18))
                      (rpn.ast/add-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/multiply-AST
                            (rpn.ast/number-AST 0.6)
                            (rpn.ast/divide-AST
                              (rpn.ast/multiply-AST
                                (rpn.ast/min-AST
                                  (rpn.ast/number-AST 1.23)
                                  (rpn.ast/symbol-AST "yT"))
                                (rpn.ast/symbol-AST "Xf"))
                              (rpn.ast/symbol-AST "iA")))
                          (rpn.ast/symbol-AST "JG"))
                        (rpn.ast/max-AST
                          (rpn.ast/symbol-AST "XB")
                          (rpn.ast/symbol-AST "Ma"))))
                    (rpn.ast/number-AST 1.42))
                  (rpn.ast/subtract-AST
                    (rpn.ast/number-AST 0.05)
                    (rpn.ast/divide-AST
                      (rpn.ast/symbol-AST "Bb")
                      (rpn.ast/symbol-AST "so"))))
                (rpn.ast/symbol-AST "gk"))
              (rpn.ast/max-AST
                (rpn.ast/number-AST 0.09)
                (rpn.ast/symbol-AST "mq")))
            (rpn.ast/symbol-AST "gj"))
          (rpn.ast/min-AST
            (rpn.ast/power-AST
              (rpn.ast/multiply-AST
                (rpn.ast/symbol-AST "EA")
                (rpn.ast/min-AST
                  (rpn.ast/symbol-AST "uk")
                  (rpn.ast/number-AST 0.77)))
              (rpn.ast/symbol-AST "CY"))
            (rpn.ast/multiply-AST
              (rpn.ast/min-AST
                (rpn.ast/symbol-AST "Fi")
                (rpn.ast/number-AST 0.62))
              (rpn.ast/number-AST 0.41))))
        (rpn.ast/number-AST 0.62))]
    ["aF * 1.36 max ( Gc * ( ( ( uv ) * 1.22 + 0.05 + ( ( 0.86 * ( ( 1.74 % 0.25 % 1.80 min ( CR ) ) max ( 0.26 / 1.36 * ( ( ( aO + 0.43 + bu min EX ) % ( ( 0.89 ) max LG ^ xe max We ) ) ^ ( ( 1.74 min 1.60 ) + Ut / ( ( KL * ( ( ( ( KZ ) ) ) / ( dp ^ 0.13 - 1.60 ) ) * ( ( ( oT ) % yz / 0.74 % KQ ) * 1.47 % 0.56 ) ) ) ) / ( 0.93 + 0.56 % ( ( cc % HE max cO ) ) * ( 1.18 + ( XA % hW ) - ( QT ) ) ) min 1.70 ) ) % ( st + 1.30 ) + 1.52 ) ) min ( OJ + 0.26 / sq max 1.09 ) min 1.16 ) ) ^ Xh ) ^ pR / kf )"
      (rpn.ast/multiply-AST
        (rpn.ast/symbol-AST "aF")
        (rpn.ast/max-AST
          (rpn.ast/number-AST 1.36)
          (rpn.ast/divide-AST
            (rpn.ast/power-AST
              (rpn.ast/multiply-AST
                (rpn.ast/symbol-AST "Gc")
                (rpn.ast/power-AST
                  (rpn.ast/add-AST
                    (rpn.ast/add-AST
                      (rpn.ast/multiply-AST
                        (rpn.ast/symbol-AST "uv")
                        (rpn.ast/number-AST 1.22))
                      (rpn.ast/number-AST 0.05))
                    (rpn.ast/min-AST
                      (rpn.ast/min-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/number-AST 0.86)
                          (rpn.ast/add-AST
                            (rpn.ast/modulo-AST
                              (rpn.ast/max-AST
                                (rpn.ast/modulo-AST
                                  (rpn.ast/modulo-AST
                                    (rpn.ast/number-AST 1.74)
                                    (rpn.ast/number-AST 0.25))
                                  (rpn.ast/min-AST
                                    (rpn.ast/number-AST 1.8)
                                    (rpn.ast/symbol-AST "CR")))
                                (rpn.ast/multiply-AST
                                  (rpn.ast/divide-AST
                                    (rpn.ast/number-AST 0.26)
                                    (rpn.ast/number-AST 1.36))
                                  (rpn.ast/divide-AST
                                    (rpn.ast/power-AST
                                      (rpn.ast/modulo-AST
                                        (rpn.ast/add-AST
                                          (rpn.ast/add-AST
                                            (rpn.ast/symbol-AST "aO")
                                            (rpn.ast/number-AST 0.43))
                                          (rpn.ast/min-AST
                                            (rpn.ast/symbol-AST "bu")
                                            (rpn.ast/symbol-AST "EX")))
                                        (rpn.ast/power-AST
                                          (rpn.ast/max-AST
                                            (rpn.ast/number-AST 0.89)
                                            (rpn.ast/symbol-AST "LG"))
                                          (rpn.ast/max-AST
                                            (rpn.ast/symbol-AST "xe")
                                            (rpn.ast/symbol-AST "We"))))
                                      (rpn.ast/add-AST
                                        (rpn.ast/min-AST
                                          (rpn.ast/number-AST 1.74)
                                          (rpn.ast/number-AST 1.6))
                                        (rpn.ast/divide-AST
                                          (rpn.ast/symbol-AST "Ut")
                                          (rpn.ast/multiply-AST
                                            (rpn.ast/multiply-AST
                                              (rpn.ast/symbol-AST "KL")
                                              (rpn.ast/divide-AST
                                                (rpn.ast/symbol-AST "KZ")
                                                (rpn.ast/subtract-AST
                                                  (rpn.ast/power-AST
                                                    (rpn.ast/symbol-AST "dp")
                                                    (rpn.ast/number-AST 0.13))
                                                  (rpn.ast/number-AST 1.6))))
                                            (rpn.ast/modulo-AST
                                              (rpn.ast/multiply-AST
                                                (rpn.ast/modulo-AST
                                                  (rpn.ast/divide-AST
                                                    (rpn.ast/modulo-AST
                                                      (rpn.ast/symbol-AST "oT")
                                                      (rpn.ast/symbol-AST "yz"))
                                                    (rpn.ast/number-AST 0.74))
                                                  (rpn.ast/symbol-AST "KQ"))
                                                (rpn.ast/number-AST 1.47))
                                              (rpn.ast/number-AST 0.56))))))
                                    (rpn.ast/min-AST
                                      (rpn.ast/add-AST
                                        (rpn.ast/number-AST 0.93)
                                        (rpn.ast/multiply-AST
                                          (rpn.ast/modulo-AST
                                            (rpn.ast/number-AST 0.56)
                                            (rpn.ast/modulo-AST
                                              (rpn.ast/symbol-AST "cc")
                                              (rpn.ast/max-AST
                                                (rpn.ast/symbol-AST "HE")
                                                (rpn.ast/symbol-AST "cO"))))
                                          (rpn.ast/subtract-AST
                                            (rpn.ast/add-AST
                                              (rpn.ast/number-AST 1.18)
                                              (rpn.ast/modulo-AST
                                                (rpn.ast/symbol-AST "XA")
                                                (rpn.ast/symbol-AST "hW")))
                                            (rpn.ast/symbol-AST "QT"))))
                                      (rpn.ast/number-AST 1.7)))))
                              (rpn.ast/add-AST
                                (rpn.ast/symbol-AST "st")
                                (rpn.ast/number-AST 1.3)))
                            (rpn.ast/number-AST 1.52)))
                        (rpn.ast/add-AST
                          (rpn.ast/symbol-AST "OJ")
                          (rpn.ast/divide-AST
                            (rpn.ast/number-AST 0.26)
                            (rpn.ast/max-AST
                              (rpn.ast/symbol-AST "sq")
                              (rpn.ast/number-AST 1.09)))))
                      (rpn.ast/number-AST 1.16)))
                  (rpn.ast/symbol-AST "Xh")))
              (rpn.ast/symbol-AST "pR"))
            (rpn.ast/symbol-AST "kf"))))]
    ["( ( ( qn ) - 0.72 * ( 1.24 ) max Hj ) + ( ( oe max ( VP / zC + 1.42 ) max 0.08 ^ 0.63 ) ) - ( ( zX % BE min 1.16 ) min ( zI ) ) ) ^ 1.75 max ( ( WZ ) max ( rm / ( BG min Pm / 1.65 ) max ( Ug ^ 0.74 ) ) + nC ) / ( 1.91 min 0.04 ^ 0.54 max ( ( 1.70 max 0.64 min kB / 0.51 ) * ( CU + 1.52 max 0.92 ^ ( ( 1.27 * Ey max ( Vm + vA % ( 1.24 - ( Eu max ( bK min Ny min ( uo min 0.24 - It ) * 1.10 ) ) ) ) ) * 1.94 + eN / 0.88 ) ) max 1.54 ^ 0.67 ) )"
      (rpn.ast/divide-AST
        (rpn.ast/power-AST
          (rpn.ast/subtract-AST
            (rpn.ast/add-AST
              (rpn.ast/subtract-AST
                (rpn.ast/symbol-AST "qn")
                (rpn.ast/multiply-AST
                  (rpn.ast/number-AST 0.72)
                  (rpn.ast/max-AST
                    (rpn.ast/number-AST 1.24)
                    (rpn.ast/symbol-AST "Hj"))))
              (rpn.ast/power-AST
                (rpn.ast/max-AST
                  (rpn.ast/max-AST
                    (rpn.ast/symbol-AST "oe")
                    (rpn.ast/add-AST
                      (rpn.ast/divide-AST
                        (rpn.ast/symbol-AST "VP")
                        (rpn.ast/symbol-AST "zC"))
                      (rpn.ast/number-AST 1.42)))
                  (rpn.ast/number-AST 0.08))
                (rpn.ast/number-AST 0.63)))
            (rpn.ast/min-AST
              (rpn.ast/modulo-AST
                (rpn.ast/symbol-AST "zX")
                (rpn.ast/min-AST
                  (rpn.ast/symbol-AST "BE")
                  (rpn.ast/number-AST 1.16)))
              (rpn.ast/symbol-AST "zI")))
          (rpn.ast/max-AST
            (rpn.ast/number-AST 1.75)
            (rpn.ast/add-AST
              (rpn.ast/max-AST
                (rpn.ast/symbol-AST "WZ")
                (rpn.ast/divide-AST
                  (rpn.ast/symbol-AST "rm")
                  (rpn.ast/max-AST
                    (rpn.ast/divide-AST
                      (rpn.ast/min-AST
                        (rpn.ast/symbol-AST "BG")
                        (rpn.ast/symbol-AST "Pm"))
                      (rpn.ast/number-AST 1.65))
                    (rpn.ast/power-AST
                      (rpn.ast/symbol-AST "Ug")
                      (rpn.ast/number-AST 0.74)))))
              (rpn.ast/symbol-AST "nC"))))
        (rpn.ast/power-AST
          (rpn.ast/min-AST
            (rpn.ast/number-AST 1.91)
            (rpn.ast/number-AST 0.04))
          (rpn.ast/max-AST
            (rpn.ast/number-AST 0.54)
            (rpn.ast/power-AST
              (rpn.ast/multiply-AST
                (rpn.ast/divide-AST
                  (rpn.ast/min-AST
                    (rpn.ast/max-AST
                      (rpn.ast/number-AST 1.7)
                      (rpn.ast/number-AST 0.64))
                    (rpn.ast/symbol-AST "kB"))
                  (rpn.ast/number-AST 0.51))
                (rpn.ast/max-AST
                  (rpn.ast/add-AST
                    (rpn.ast/symbol-AST "CU")
                    (rpn.ast/power-AST
                      (rpn.ast/max-AST
                        (rpn.ast/number-AST 1.52)
                        (rpn.ast/number-AST 0.92))
                      (rpn.ast/add-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/multiply-AST
                            (rpn.ast/number-AST 1.27)
                            (rpn.ast/max-AST
                              (rpn.ast/symbol-AST "Ey")
                              (rpn.ast/add-AST
                                (rpn.ast/symbol-AST "Vm")
                                (rpn.ast/modulo-AST
                                  (rpn.ast/symbol-AST "vA")
                                  (rpn.ast/subtract-AST
                                    (rpn.ast/number-AST 1.24)
                                    (rpn.ast/max-AST
                                      (rpn.ast/symbol-AST "Eu")
                                      (rpn.ast/multiply-AST
                                        (rpn.ast/min-AST
                                          (rpn.ast/min-AST
                                            (rpn.ast/symbol-AST "bK")
                                            (rpn.ast/symbol-AST "Ny"))
                                          (rpn.ast/subtract-AST
                                            (rpn.ast/min-AST
                                              (rpn.ast/symbol-AST "uo")
                                              (rpn.ast/number-AST 0.24))
                                            (rpn.ast/symbol-AST "It")))
                                        (rpn.ast/number-AST 1.1))))))))
                          (rpn.ast/number-AST 1.94))
                        (rpn.ast/divide-AST
                          (rpn.ast/symbol-AST "eN")
                          (rpn.ast/number-AST 0.88)))))
                  (rpn.ast/number-AST 1.54)))
              (rpn.ast/number-AST 0.67)))))]
    ["( NU ) - ( ( zL % FE + ( ( 0.80 min ( 1.26 * SP / ( ( 1.47 max 0.15 - NP + ( ( 1.34 - uU ) * 1.91 ) ) ) ) ) ) ^ 0.30 ) + 0.69 * Jw ^ 1.62 )"
      (rpn.ast/subtract-AST
        (rpn.ast/symbol-AST "NU")
        (rpn.ast/add-AST
          (rpn.ast/add-AST
            (rpn.ast/modulo-AST
              (rpn.ast/symbol-AST "zL")
              (rpn.ast/symbol-AST "FE"))
            (rpn.ast/power-AST
              (rpn.ast/min-AST
                (rpn.ast/number-AST 0.8)
                (rpn.ast/divide-AST
                  (rpn.ast/multiply-AST
                    (rpn.ast/number-AST 1.26)
                    (rpn.ast/symbol-AST "SP"))
                  (rpn.ast/add-AST
                    (rpn.ast/subtract-AST
                      (rpn.ast/max-AST
                        (rpn.ast/number-AST 1.47)
                        (rpn.ast/number-AST 0.15))
                      (rpn.ast/symbol-AST "NP"))
                    (rpn.ast/multiply-AST
                      (rpn.ast/subtract-AST
                        (rpn.ast/number-AST 1.34)
                        (rpn.ast/symbol-AST "uU"))
                      (rpn.ast/number-AST 1.91)))))
              (rpn.ast/number-AST 0.3)))
          (rpn.ast/power-AST
            (rpn.ast/multiply-AST
              (rpn.ast/number-AST 0.69)
              (rpn.ast/symbol-AST "Jw"))
            (rpn.ast/number-AST 1.62))))]
    ["xH % uU * 1.86 % 0.39"
      (rpn.ast/modulo-AST
        (rpn.ast/multiply-AST
          (rpn.ast/modulo-AST
            (rpn.ast/symbol-AST "xH")
            (rpn.ast/symbol-AST "uU"))
          (rpn.ast/number-AST 1.86))
        (rpn.ast/number-AST 0.39))]
    ["jY"
      (rpn.ast/symbol-AST "jY")]
    ["QJ"
      (rpn.ast/symbol-AST "QJ")]
    ["( ( 1.92 + nR ) % lr - ( ( 1.94 * 1.34 min AQ ^ 1.98 ) + ( ( ( ( ( ( 1.82 min ( 1.00 + ( Ys - ( eE ^ ( jv + ( 1.52 ) * aO ) ) ^ oz ) ) * ( ( FF ) + tE ) min ( NB / Rz ) ) / 0.76 ) ^ TV max Fu min 0.46 ) * Cr + HR ^ Iv ) max 1.97 % ( ( 1.25 % ( 0.14 ) * ( Kc ) % 0.86 ) - 1.47 + ( ( 1.54 + 1.52 / ( FP - cp max ( 1.66 / zM ) min 1.47 ) * 1.01 ) ^ ( Ow ) min AY ) ) ^ ( ( 0.24 ) % ( ss ) ^ ( cL ^ Bm ) % 1.38 ) ) min 0.01 - 1.12 ) ) max 0.30 ) + ts % 1.72"
      (rpn.ast/add-AST
        (rpn.ast/subtract-AST
          (rpn.ast/modulo-AST
            (rpn.ast/add-AST
              (rpn.ast/number-AST 1.92)
              (rpn.ast/symbol-AST "nR"))
            (rpn.ast/symbol-AST "lr"))
          (rpn.ast/max-AST
            (rpn.ast/add-AST
              (rpn.ast/power-AST
                (rpn.ast/multiply-AST
                  (rpn.ast/number-AST 1.94)
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 1.34)
                    (rpn.ast/symbol-AST "AQ")))
                (rpn.ast/number-AST 1.98))
              (rpn.ast/subtract-AST
                (rpn.ast/min-AST
                  (rpn.ast/power-AST
                    (rpn.ast/modulo-AST
                      (rpn.ast/max-AST
                        (rpn.ast/add-AST
                          (rpn.ast/multiply-AST
                            (rpn.ast/power-AST
                              (rpn.ast/divide-AST
                                (rpn.ast/multiply-AST
                                  (rpn.ast/min-AST
                                    (rpn.ast/number-AST 1.82)
                                    (rpn.ast/add-AST
                                      (rpn.ast/number-AST 1.0)
                                      (rpn.ast/subtract-AST
                                        (rpn.ast/symbol-AST "Ys")
                                        (rpn.ast/power-AST
                                          (rpn.ast/power-AST
                                            (rpn.ast/symbol-AST "eE")
                                            (rpn.ast/add-AST
                                              (rpn.ast/symbol-AST "jv")
                                              (rpn.ast/multiply-AST
                                                (rpn.ast/number-AST 1.52)
                                                (rpn.ast/symbol-AST "aO"))))
                                          (rpn.ast/symbol-AST "oz")))))
                                  (rpn.ast/min-AST
                                    (rpn.ast/add-AST
                                      (rpn.ast/symbol-AST "FF")
                                      (rpn.ast/symbol-AST "tE"))
                                    (rpn.ast/divide-AST
                                      (rpn.ast/symbol-AST "NB")
                                      (rpn.ast/symbol-AST "Rz"))))
                                (rpn.ast/number-AST 0.76))
                              (rpn.ast/min-AST
                                (rpn.ast/max-AST
                                  (rpn.ast/symbol-AST "TV")
                                  (rpn.ast/symbol-AST "Fu"))
                                (rpn.ast/number-AST 0.46)))
                            (rpn.ast/symbol-AST "Cr"))
                          (rpn.ast/power-AST
                            (rpn.ast/symbol-AST "HR")
                            (rpn.ast/symbol-AST "Iv")))
                        (rpn.ast/number-AST 1.97))
                      (rpn.ast/add-AST
                        (rpn.ast/subtract-AST
                          (rpn.ast/modulo-AST
                            (rpn.ast/multiply-AST
                              (rpn.ast/modulo-AST
                                (rpn.ast/number-AST 1.25)
                                (rpn.ast/number-AST 0.14))
                              (rpn.ast/symbol-AST "Kc"))
                            (rpn.ast/number-AST 0.86))
                          (rpn.ast/number-AST 1.47))
                        (rpn.ast/power-AST
                          (rpn.ast/add-AST
                            (rpn.ast/number-AST 1.54)
                            (rpn.ast/multiply-AST
                              (rpn.ast/divide-AST
                                (rpn.ast/number-AST 1.52)
                                (rpn.ast/subtract-AST
                                  (rpn.ast/symbol-AST "FP")
                                  (rpn.ast/min-AST
                                    (rpn.ast/max-AST
                                      (rpn.ast/symbol-AST "cp")
                                      (rpn.ast/divide-AST
                                        (rpn.ast/number-AST 1.66)
                                        (rpn.ast/symbol-AST "zM")))
                                    (rpn.ast/number-AST 1.47))))
                              (rpn.ast/number-AST 1.01)))
                          (rpn.ast/min-AST
                            (rpn.ast/symbol-AST "Ow")
                            (rpn.ast/symbol-AST "AY")))))
                    (rpn.ast/modulo-AST
                      (rpn.ast/power-AST
                        (rpn.ast/modulo-AST
                          (rpn.ast/number-AST 0.24)
                          (rpn.ast/symbol-AST "ss"))
                        (rpn.ast/power-AST
                          (rpn.ast/symbol-AST "cL")
                          (rpn.ast/symbol-AST "Bm")))
                      (rpn.ast/number-AST 1.38)))
                  (rpn.ast/number-AST 0.01))
                (rpn.ast/number-AST 1.12)))
            (rpn.ast/number-AST 0.3)))
        (rpn.ast/modulo-AST
          (rpn.ast/symbol-AST "ts")
          (rpn.ast/number-AST 1.72)))]
    ["0.29 ^ ( ( 1.11 / rH + Xa ^ 1.33 ) max oy max 1.45 max Xr ) + ( NL - 1.51 )"
      (rpn.ast/add-AST
        (rpn.ast/power-AST
          (rpn.ast/number-AST 0.29)
          (rpn.ast/max-AST
            (rpn.ast/max-AST
              (rpn.ast/max-AST
                (rpn.ast/add-AST
                  (rpn.ast/divide-AST
                    (rpn.ast/number-AST 1.11)
                    (rpn.ast/symbol-AST "rH"))
                  (rpn.ast/power-AST
                    (rpn.ast/symbol-AST "Xa")
                    (rpn.ast/number-AST 1.33)))
                (rpn.ast/symbol-AST "oy"))
              (rpn.ast/number-AST 1.45))
            (rpn.ast/symbol-AST "Xr")))
        (rpn.ast/subtract-AST
          (rpn.ast/symbol-AST "NL")
          (rpn.ast/number-AST 1.51)))]
    ["0.18 min 0.43 * 0.06"
      (rpn.ast/multiply-AST
        (rpn.ast/min-AST
          (rpn.ast/number-AST 0.18)
          (rpn.ast/number-AST 0.43))
        (rpn.ast/number-AST 0.06))]
    ["( ( ( ( wW min to + 0.24 % Uv ) ^ Mg / 1.26 ) ) % ( 1.73 ) - Qr ) / ( HA - Pn - 0.75 ^ VW )"
      (rpn.ast/divide-AST
        (rpn.ast/subtract-AST
          (rpn.ast/modulo-AST
            (rpn.ast/divide-AST
              (rpn.ast/power-AST
                (rpn.ast/add-AST
                  (rpn.ast/min-AST
                    (rpn.ast/symbol-AST "wW")
                    (rpn.ast/symbol-AST "to"))
                  (rpn.ast/modulo-AST
                    (rpn.ast/number-AST 0.24)
                    (rpn.ast/symbol-AST "Uv")))
                (rpn.ast/symbol-AST "Mg"))
              (rpn.ast/number-AST 1.26))
            (rpn.ast/number-AST 1.73))
          (rpn.ast/symbol-AST "Qr"))
        (rpn.ast/subtract-AST
          (rpn.ast/subtract-AST
            (rpn.ast/symbol-AST "HA")
            (rpn.ast/symbol-AST "Pn"))
          (rpn.ast/power-AST
            (rpn.ast/number-AST 0.75)
            (rpn.ast/symbol-AST "VW"))))]
    ["Jj"
      (rpn.ast/symbol-AST "Jj")]
    ["MI + ( 1.72 % ( Xj ^ ( 0.97 min 1.05 ^ gA ) % QA max Qq ) ^ zB + 1.46 )"
      (rpn.ast/add-AST
        (rpn.ast/symbol-AST "MI")
        (rpn.ast/add-AST
          (rpn.ast/power-AST
            (rpn.ast/modulo-AST
              (rpn.ast/number-AST 1.72)
              (rpn.ast/modulo-AST
                (rpn.ast/power-AST
                  (rpn.ast/symbol-AST "Xj")
                  (rpn.ast/power-AST
                    (rpn.ast/min-AST
                      (rpn.ast/number-AST 0.97)
                      (rpn.ast/number-AST 1.05))
                    (rpn.ast/symbol-AST "gA")))
                (rpn.ast/max-AST
                  (rpn.ast/symbol-AST "QA")
                  (rpn.ast/symbol-AST "Qq"))))
            (rpn.ast/symbol-AST "zB"))
          (rpn.ast/number-AST 1.46)))]
    ["( 1.54 max ( 1.47 min 1.21 + 1.04 ) % 1.51 + wO ) + gN ^ 1.71 * ge"
      (rpn.ast/add-AST
        (rpn.ast/add-AST
          (rpn.ast/modulo-AST
            (rpn.ast/max-AST
              (rpn.ast/number-AST 1.54)
              (rpn.ast/add-AST
                (rpn.ast/min-AST
                  (rpn.ast/number-AST 1.47)
                  (rpn.ast/number-AST 1.21))
                (rpn.ast/number-AST 1.04)))
            (rpn.ast/number-AST 1.51))
          (rpn.ast/symbol-AST "wO"))
        (rpn.ast/multiply-AST
          (rpn.ast/power-AST
            (rpn.ast/symbol-AST "gN")
            (rpn.ast/number-AST 1.71))
          (rpn.ast/symbol-AST "ge")))]
    ["( 0.03 / ( ( 0.98 % hN - DI ) ) min od ) + 1.39 % ( ph min sX ) % 0.38"
      (rpn.ast/add-AST
        (rpn.ast/divide-AST
          (rpn.ast/number-AST 0.03)
          (rpn.ast/min-AST
            (rpn.ast/subtract-AST
              (rpn.ast/modulo-AST
                (rpn.ast/number-AST 0.98)
                (rpn.ast/symbol-AST "hN"))
              (rpn.ast/symbol-AST "DI"))
            (rpn.ast/symbol-AST "od")))
        (rpn.ast/modulo-AST
          (rpn.ast/modulo-AST
            (rpn.ast/number-AST 1.39)
            (rpn.ast/min-AST
              (rpn.ast/symbol-AST "ph")
              (rpn.ast/symbol-AST "sX")))
          (rpn.ast/number-AST 0.38)))]
    ["( WH ^ 1.85 )"
      (rpn.ast/power-AST
        (rpn.ast/symbol-AST "WH")
        (rpn.ast/number-AST 1.85))]
    ["( Gq % OA / ew )"
      (rpn.ast/divide-AST
        (rpn.ast/modulo-AST
          (rpn.ast/symbol-AST "Gq")
          (rpn.ast/symbol-AST "OA"))
        (rpn.ast/symbol-AST "ew"))]
    ["dO * ( 0.16 max 1.21 % QF ^ qa ) / BS"
      (rpn.ast/divide-AST
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "dO")
          (rpn.ast/power-AST
            (rpn.ast/modulo-AST
              (rpn.ast/max-AST
                (rpn.ast/number-AST 0.16)
                (rpn.ast/number-AST 1.21))
              (rpn.ast/symbol-AST "QF"))
            (rpn.ast/symbol-AST "qa")))
        (rpn.ast/symbol-AST "BS"))]
    ["wX - 0.77"
      (rpn.ast/subtract-AST
        (rpn.ast/symbol-AST "wX")
        (rpn.ast/number-AST 0.77))]
    ["( ( Ay * ( 0.50 ) max BA ^ ( 1.80 ) ) - 0.64 ) * ( Ui / 1.69 - xC ) % ( UN + 1.70 min 0.02 min JR ) / 1.85"
      (rpn.ast/divide-AST
        (rpn.ast/modulo-AST
          (rpn.ast/multiply-AST
            (rpn.ast/subtract-AST
              (rpn.ast/power-AST
                (rpn.ast/multiply-AST
                  (rpn.ast/symbol-AST "Ay")
                  (rpn.ast/max-AST
                    (rpn.ast/number-AST 0.5)
                    (rpn.ast/symbol-AST "BA")))
                (rpn.ast/number-AST 1.8))
              (rpn.ast/number-AST 0.64))
            (rpn.ast/subtract-AST
              (rpn.ast/divide-AST
                (rpn.ast/symbol-AST "Ui")
                (rpn.ast/number-AST 1.69))
              (rpn.ast/symbol-AST "xC")))
          (rpn.ast/add-AST
            (rpn.ast/symbol-AST "UN")
            (rpn.ast/min-AST
              (rpn.ast/min-AST
                (rpn.ast/number-AST 1.7)
                (rpn.ast/number-AST 0.02))
              (rpn.ast/symbol-AST "JR"))))
        (rpn.ast/number-AST 1.85))]
    ["( it max FE / 0.73 - ( 0.33 % ( 0.15 * ( ( ( 0.01 max 1.21 + Bq % 1.69 ) - Ve ) - ( nY max 0.17 + vz / ( QC ) ) ) ) / ( 0.47 % 1.57 + ( 1.01 - 0.21 ) ) ) ) + 1.55 ^ ( 1.54 max SF + fB + ( ( ym ) ) ) - ln"
      (rpn.ast/subtract-AST
        (rpn.ast/add-AST
          (rpn.ast/subtract-AST
            (rpn.ast/divide-AST
              (rpn.ast/max-AST
                (rpn.ast/symbol-AST "it")
                (rpn.ast/symbol-AST "FE"))
              (rpn.ast/number-AST 0.73))
            (rpn.ast/divide-AST
              (rpn.ast/modulo-AST
                (rpn.ast/number-AST 0.33)
                (rpn.ast/multiply-AST
                  (rpn.ast/number-AST 0.15)
                  (rpn.ast/subtract-AST
                    (rpn.ast/subtract-AST
                      (rpn.ast/add-AST
                        (rpn.ast/max-AST
                          (rpn.ast/number-AST 0.01)
                          (rpn.ast/number-AST 1.21))
                        (rpn.ast/modulo-AST
                          (rpn.ast/symbol-AST "Bq")
                          (rpn.ast/number-AST 1.69)))
                      (rpn.ast/symbol-AST "Ve"))
                    (rpn.ast/add-AST
                      (rpn.ast/max-AST
                        (rpn.ast/symbol-AST "nY")
                        (rpn.ast/number-AST 0.17))
                      (rpn.ast/divide-AST
                        (rpn.ast/symbol-AST "vz")
                        (rpn.ast/symbol-AST "QC"))))))
              (rpn.ast/add-AST
                (rpn.ast/modulo-AST
                  (rpn.ast/number-AST 0.47)
                  (rpn.ast/number-AST 1.57))
                (rpn.ast/subtract-AST
                  (rpn.ast/number-AST 1.01)
                  (rpn.ast/number-AST 0.21)))))
          (rpn.ast/power-AST
            (rpn.ast/number-AST 1.55)
            (rpn.ast/add-AST
              (rpn.ast/add-AST
                (rpn.ast/max-AST
                  (rpn.ast/number-AST 1.54)
                  (rpn.ast/symbol-AST "SF"))
                (rpn.ast/symbol-AST "fB"))
              (rpn.ast/symbol-AST "ym"))))
        (rpn.ast/symbol-AST "ln"))]
    ["0.90"
      (rpn.ast/number-AST 0.9)]
    ["OP"
      (rpn.ast/symbol-AST "OP")]
    ["0.38 / ( rU / uh % 0.25 )"
      (rpn.ast/divide-AST
        (rpn.ast/number-AST 0.38)
        (rpn.ast/modulo-AST
          (rpn.ast/divide-AST
            (rpn.ast/symbol-AST "rU")
            (rpn.ast/symbol-AST "uh"))
          (rpn.ast/number-AST 0.25)))]
    ["( uz )"
      (rpn.ast/symbol-AST "uz")]
    ["vF ^ ( jw ) / kR"
      (rpn.ast/divide-AST
        (rpn.ast/power-AST
          (rpn.ast/symbol-AST "vF")
          (rpn.ast/symbol-AST "jw"))
        (rpn.ast/symbol-AST "kR"))]
    ["gk"
      (rpn.ast/symbol-AST "gk")]
    ["( ( BU ) max ( lo ^ VY ) * ( ( ( 1.94 % 0.62 % ( 0.51 % Qy max ( ( Ag ^ wh - 0.19 ) ) min ( ( gP ^ ( 1.96 ) ) min Ur ) ) ) + ( 0.61 % ( HZ ) * ( 0.56 min dr ^ 0.64 ) ) ) * cu max OU min 1.62 ) + 1.74 )"
      (rpn.ast/add-AST
        (rpn.ast/multiply-AST
          (rpn.ast/max-AST
            (rpn.ast/symbol-AST "BU")
            (rpn.ast/power-AST
              (rpn.ast/symbol-AST "lo")
              (rpn.ast/symbol-AST "VY")))
          (rpn.ast/multiply-AST
            (rpn.ast/add-AST
              (rpn.ast/modulo-AST
                (rpn.ast/modulo-AST
                  (rpn.ast/number-AST 1.94)
                  (rpn.ast/number-AST 0.62))
                (rpn.ast/modulo-AST
                  (rpn.ast/number-AST 0.51)
                  (rpn.ast/min-AST
                    (rpn.ast/max-AST
                      (rpn.ast/symbol-AST "Qy")
                      (rpn.ast/subtract-AST
                        (rpn.ast/power-AST
                          (rpn.ast/symbol-AST "Ag")
                          (rpn.ast/symbol-AST "wh"))
                        (rpn.ast/number-AST 0.19)))
                    (rpn.ast/min-AST
                      (rpn.ast/power-AST
                        (rpn.ast/symbol-AST "gP")
                        (rpn.ast/number-AST 1.96))
                      (rpn.ast/symbol-AST "Ur")))))
              (rpn.ast/multiply-AST
                (rpn.ast/modulo-AST
                  (rpn.ast/number-AST 0.61)
                  (rpn.ast/symbol-AST "HZ"))
                (rpn.ast/power-AST
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 0.56)
                    (rpn.ast/symbol-AST "dr"))
                  (rpn.ast/number-AST 0.64))))
            (rpn.ast/min-AST
              (rpn.ast/max-AST
                (rpn.ast/symbol-AST "cu")
                (rpn.ast/symbol-AST "OU"))
              (rpn.ast/number-AST 1.62))))
        (rpn.ast/number-AST 1.74))]
    ["( ft ) / Jh + Gt * AP"
      (rpn.ast/add-AST
        (rpn.ast/divide-AST
          (rpn.ast/symbol-AST "ft")
          (rpn.ast/symbol-AST "Jh"))
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "Gt")
          (rpn.ast/symbol-AST "AP")))]
    ["Zw - CA + Co"
      (rpn.ast/add-AST
        (rpn.ast/subtract-AST
          (rpn.ast/symbol-AST "Zw")
          (rpn.ast/symbol-AST "CA"))
        (rpn.ast/symbol-AST "Co"))]
    ["( 1.27 ) - NT"
      (rpn.ast/subtract-AST
        (rpn.ast/number-AST 1.27)
        (rpn.ast/symbol-AST "NT"))]
    ["FI min KU % ( sQ * 1.84 )"
      (rpn.ast/modulo-AST
        (rpn.ast/min-AST
          (rpn.ast/symbol-AST "FI")
          (rpn.ast/symbol-AST "KU"))
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "sQ")
          (rpn.ast/number-AST 1.84)))]
    ["( 1.73 ) ^ cE / Uv"
      (rpn.ast/divide-AST
        (rpn.ast/power-AST
          (rpn.ast/number-AST 1.73)
          (rpn.ast/symbol-AST "cE"))
        (rpn.ast/symbol-AST "Uv"))]
    ["0.98 / 1.19 / ( Wk - lG % ( 1.06 ^ Mv ) % xf ) + HS"
      (rpn.ast/add-AST
        (rpn.ast/divide-AST
          (rpn.ast/divide-AST
            (rpn.ast/number-AST 0.98)
            (rpn.ast/number-AST 1.19))
          (rpn.ast/subtract-AST
            (rpn.ast/symbol-AST "Wk")
            (rpn.ast/modulo-AST
              (rpn.ast/modulo-AST
                (rpn.ast/symbol-AST "lG")
                (rpn.ast/power-AST
                  (rpn.ast/number-AST 1.06)
                  (rpn.ast/symbol-AST "Mv")))
              (rpn.ast/symbol-AST "xf"))))
        (rpn.ast/symbol-AST "HS"))]
    ["1.64 % dX"
      (rpn.ast/modulo-AST
        (rpn.ast/number-AST 1.64)
        (rpn.ast/symbol-AST "dX"))]
    ["( 1.04 max HD % nU )"
      (rpn.ast/modulo-AST
        (rpn.ast/max-AST
          (rpn.ast/number-AST 1.04)
          (rpn.ast/symbol-AST "HD"))
        (rpn.ast/symbol-AST "nU"))]
    ["0.39"
      (rpn.ast/number-AST 0.39)]
    ["1.15 - KG + 0.85 % ( ( 1.71 ^ 0.43 min uL min 0.73 ) / ( ( Mo ) - lJ % ( ( ( ( 1.20 % ip ) - ( au ) ) - 1.29 max AS max 1.21 ) ^ 1.02 ) ) - pQ )"
      (rpn.ast/add-AST
        (rpn.ast/subtract-AST
          (rpn.ast/number-AST 1.15)
          (rpn.ast/symbol-AST "KG"))
        (rpn.ast/modulo-AST
          (rpn.ast/number-AST 0.85)
          (rpn.ast/subtract-AST
            (rpn.ast/divide-AST
              (rpn.ast/power-AST
                (rpn.ast/number-AST 1.71)
                (rpn.ast/min-AST
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 0.43)
                    (rpn.ast/symbol-AST "uL"))
                  (rpn.ast/number-AST 0.73)))
              (rpn.ast/subtract-AST
                (rpn.ast/symbol-AST "Mo")
                (rpn.ast/modulo-AST
                  (rpn.ast/symbol-AST "lJ")
                  (rpn.ast/power-AST
                    (rpn.ast/subtract-AST
                      (rpn.ast/subtract-AST
                        (rpn.ast/modulo-AST
                          (rpn.ast/number-AST 1.2)
                          (rpn.ast/symbol-AST "ip"))
                        (rpn.ast/symbol-AST "au"))
                      (rpn.ast/max-AST
                        (rpn.ast/max-AST
                          (rpn.ast/number-AST 1.29)
                          (rpn.ast/symbol-AST "AS"))
                        (rpn.ast/number-AST 1.21)))
                    (rpn.ast/number-AST 1.02)))))
            (rpn.ast/symbol-AST "pQ"))))]
    ["( ( ( dp - 0.16 / 1.23 ) - 1.54 min 0.82 % ( wI % uP ) ) )"
      (rpn.ast/subtract-AST
        (rpn.ast/subtract-AST
          (rpn.ast/symbol-AST "dp")
          (rpn.ast/divide-AST
            (rpn.ast/number-AST 0.16)
            (rpn.ast/number-AST 1.23)))
        (rpn.ast/modulo-AST
          (rpn.ast/min-AST
            (rpn.ast/number-AST 1.54)
            (rpn.ast/number-AST 0.82))
          (rpn.ast/modulo-AST
            (rpn.ast/symbol-AST "wI")
            (rpn.ast/symbol-AST "uP"))))]
    ["( 1.15 max 1.04 + 1.15 ) - 1.86"
      (rpn.ast/subtract-AST
        (rpn.ast/add-AST
          (rpn.ast/max-AST
            (rpn.ast/number-AST 1.15)
            (rpn.ast/number-AST 1.04))
          (rpn.ast/number-AST 1.15))
        (rpn.ast/number-AST 1.86))]
    ["1.44 / 0.68"
      (rpn.ast/divide-AST
        (rpn.ast/number-AST 1.44)
        (rpn.ast/number-AST 0.68))]
    ["wO % NV"
      (rpn.ast/modulo-AST
        (rpn.ast/symbol-AST "wO")
        (rpn.ast/symbol-AST "NV"))]
    ["Wp * ys min 0.46 + ( ( ( ( 1.32 ^ Bw ) max 1.70 - ( ( KL min ( ( 1.23 / ( 1.92 ) / ( 0.30 ) % ( zq + bj ) ) - ( ( hO * Bz % ( Iv + ( ( YE ) % rI - 1.24 ) ) ) min IA ) % ( lO max ( ( nE ^ Mu + 1.53 ) - kl min 1.31 min hb ) * oX % ( ( 0.74 % 0.80 - aC ) ) ) ) * 1.06 ) ) min 1.68 ) / ( 0.83 - 1.71 * 0.45 ) max 0.84 min ( ( ( 1.30 % 1.50 - kb ) min 0.95 / ( 0.21 % hg ) ) max 0.20 ) ) / ( ( ( 1.83 * Ks - 0.29 ) * Ua % 1.92 % ( ( 0.24 + dL / Fn ) ) ) max 1.00 % 0.14 ) + 1.12 )"
      (rpn.ast/add-AST
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "Wp")
          (rpn.ast/min-AST
            (rpn.ast/symbol-AST "ys")
            (rpn.ast/number-AST 0.46)))
        (rpn.ast/add-AST
          (rpn.ast/divide-AST
            (rpn.ast/divide-AST
              (rpn.ast/subtract-AST
                (rpn.ast/max-AST
                  (rpn.ast/power-AST
                    (rpn.ast/number-AST 1.32)
                    (rpn.ast/symbol-AST "Bw"))
                  (rpn.ast/number-AST 1.7))
                (rpn.ast/min-AST
                  (rpn.ast/multiply-AST
                    (rpn.ast/min-AST
                      (rpn.ast/symbol-AST "KL")
                      (rpn.ast/subtract-AST
                        (rpn.ast/modulo-AST
                          (rpn.ast/divide-AST
                            (rpn.ast/divide-AST
                              (rpn.ast/number-AST 1.23)
                              (rpn.ast/number-AST 1.92))
                            (rpn.ast/number-AST 0.3))
                          (rpn.ast/add-AST
                            (rpn.ast/symbol-AST "zq")
                            (rpn.ast/symbol-AST "bj")))
                        (rpn.ast/modulo-AST
                          (rpn.ast/min-AST
                            (rpn.ast/modulo-AST
                              (rpn.ast/multiply-AST
                                (rpn.ast/symbol-AST "hO")
                                (rpn.ast/symbol-AST "Bz"))
                              (rpn.ast/add-AST
                                (rpn.ast/symbol-AST "Iv")
                                (rpn.ast/subtract-AST
                                  (rpn.ast/modulo-AST
                                    (rpn.ast/symbol-AST "YE")
                                    (rpn.ast/symbol-AST "rI"))
                                  (rpn.ast/number-AST 1.24))))
                            (rpn.ast/symbol-AST "IA"))
                          (rpn.ast/modulo-AST
                            (rpn.ast/multiply-AST
                              (rpn.ast/max-AST
                                (rpn.ast/symbol-AST "lO")
                                (rpn.ast/subtract-AST
                                  (rpn.ast/add-AST
                                    (rpn.ast/power-AST
                                      (rpn.ast/symbol-AST "nE")
                                      (rpn.ast/symbol-AST "Mu"))
                                    (rpn.ast/number-AST 1.53))
                                  (rpn.ast/min-AST
                                    (rpn.ast/min-AST
                                      (rpn.ast/symbol-AST "kl")
                                      (rpn.ast/number-AST 1.31))
                                    (rpn.ast/symbol-AST "hb"))))
                              (rpn.ast/symbol-AST "oX"))
                            (rpn.ast/subtract-AST
                              (rpn.ast/modulo-AST
                                (rpn.ast/number-AST 0.74)
                                (rpn.ast/number-AST 0.8))
                              (rpn.ast/symbol-AST "aC"))))))
                    (rpn.ast/number-AST 1.06))
                  (rpn.ast/number-AST 1.68)))
              (rpn.ast/min-AST
                (rpn.ast/max-AST
                  (rpn.ast/subtract-AST
                    (rpn.ast/number-AST 0.83)
                    (rpn.ast/multiply-AST
                      (rpn.ast/number-AST 1.71)
                      (rpn.ast/number-AST 0.45)))
                  (rpn.ast/number-AST 0.84))
                (rpn.ast/max-AST
                  (rpn.ast/divide-AST
                    (rpn.ast/min-AST
                      (rpn.ast/subtract-AST
                        (rpn.ast/modulo-AST
                          (rpn.ast/number-AST 1.3)
                          (rpn.ast/number-AST 1.5))
                        (rpn.ast/symbol-AST "kb"))
                      (rpn.ast/number-AST 0.95))
                    (rpn.ast/modulo-AST
                      (rpn.ast/number-AST 0.21)
                      (rpn.ast/symbol-AST "hg")))
                  (rpn.ast/number-AST 0.2))))
            (rpn.ast/modulo-AST
              (rpn.ast/max-AST
                (rpn.ast/modulo-AST
                  (rpn.ast/modulo-AST
                    (rpn.ast/multiply-AST
                      (rpn.ast/subtract-AST
                        (rpn.ast/multiply-AST
                          (rpn.ast/number-AST 1.83)
                          (rpn.ast/symbol-AST "Ks"))
                        (rpn.ast/number-AST 0.29))
                      (rpn.ast/symbol-AST "Ua"))
                    (rpn.ast/number-AST 1.92))
                  (rpn.ast/add-AST
                    (rpn.ast/number-AST 0.24)
                    (rpn.ast/divide-AST
                      (rpn.ast/symbol-AST "dL")
                      (rpn.ast/symbol-AST "Fn"))))
                (rpn.ast/number-AST 1.0))
              (rpn.ast/number-AST 0.14)))
          (rpn.ast/number-AST 1.12)))]
    ["JB"
      (rpn.ast/symbol-AST "JB")]
    ["aY"
      (rpn.ast/symbol-AST "aY")]
    ["0.92 * ( FX ) / Ml / ( 0.89 % ( Or max wM ) max 0.01 )"
      (rpn.ast/divide-AST
        (rpn.ast/divide-AST
          (rpn.ast/multiply-AST
            (rpn.ast/number-AST 0.92)
            (rpn.ast/symbol-AST "FX"))
          (rpn.ast/symbol-AST "Ml"))
        (rpn.ast/modulo-AST
          (rpn.ast/number-AST 0.89)
          (rpn.ast/max-AST
            (rpn.ast/max-AST
              (rpn.ast/symbol-AST "Or")
              (rpn.ast/symbol-AST "wM"))
            (rpn.ast/number-AST 0.01))))]
    ["( tL + 1.37 ) + ( ( Pw + 0.87 - ( uC * Bb * no min ( 1.88 ^ ( 1.09 max ( ( 0.15 min ( 0.80 ) ^ uM max 1.99 ) * ( 0.17 ^ 0.54 min 0.24 + 0.27 ) - Ee * dd ) + 1.52 ^ vG ) + ( ( ( ( uU / tD ) * ( HT ^ 1.62 % cR - 0.14 ) ) min Ie / 1.84 ) * Zf % jO ) ) ) ) ) + ( CT min la * LH )"
      (rpn.ast/add-AST
        (rpn.ast/add-AST
          (rpn.ast/add-AST
            (rpn.ast/symbol-AST "tL")
            (rpn.ast/number-AST 1.37))
          (rpn.ast/subtract-AST
            (rpn.ast/add-AST
              (rpn.ast/symbol-AST "Pw")
              (rpn.ast/number-AST 0.87))
            (rpn.ast/multiply-AST
              (rpn.ast/multiply-AST
                (rpn.ast/symbol-AST "uC")
                (rpn.ast/symbol-AST "Bb"))
              (rpn.ast/min-AST
                (rpn.ast/symbol-AST "no")
                (rpn.ast/add-AST
                  (rpn.ast/power-AST
                    (rpn.ast/number-AST 1.88)
                    (rpn.ast/add-AST
                      (rpn.ast/max-AST
                        (rpn.ast/number-AST 1.09)
                        (rpn.ast/subtract-AST
                          (rpn.ast/multiply-AST
                            (rpn.ast/power-AST
                              (rpn.ast/min-AST
                                (rpn.ast/number-AST 0.15)
                                (rpn.ast/number-AST 0.8))
                              (rpn.ast/max-AST
                                (rpn.ast/symbol-AST "uM")
                                (rpn.ast/number-AST 1.99)))
                            (rpn.ast/add-AST
                              (rpn.ast/power-AST
                                (rpn.ast/number-AST 0.17)
                                (rpn.ast/min-AST
                                  (rpn.ast/number-AST 0.54)
                                  (rpn.ast/number-AST 0.24)))
                              (rpn.ast/number-AST 0.27)))
                          (rpn.ast/multiply-AST
                            (rpn.ast/symbol-AST "Ee")
                            (rpn.ast/symbol-AST "dd"))))
                      (rpn.ast/power-AST
                        (rpn.ast/number-AST 1.52)
                        (rpn.ast/symbol-AST "vG"))))
                  (rpn.ast/modulo-AST
                    (rpn.ast/multiply-AST
                      (rpn.ast/divide-AST
                        (rpn.ast/min-AST
                          (rpn.ast/multiply-AST
                            (rpn.ast/divide-AST
                              (rpn.ast/symbol-AST "uU")
                              (rpn.ast/symbol-AST "tD"))
                            (rpn.ast/subtract-AST
                              (rpn.ast/modulo-AST
                                (rpn.ast/power-AST
                                  (rpn.ast/symbol-AST "HT")
                                  (rpn.ast/number-AST 1.62))
                                (rpn.ast/symbol-AST "cR"))
                              (rpn.ast/number-AST 0.14)))
                          (rpn.ast/symbol-AST "Ie"))
                        (rpn.ast/number-AST 1.84))
                      (rpn.ast/symbol-AST "Zf"))
                    (rpn.ast/symbol-AST "jO")))))))
        (rpn.ast/multiply-AST
          (rpn.ast/min-AST
            (rpn.ast/symbol-AST "CT")
            (rpn.ast/symbol-AST "la"))
          (rpn.ast/symbol-AST "LH")))]
    ["Jo % Qs"
      (rpn.ast/modulo-AST
        (rpn.ast/symbol-AST "Jo")
        (rpn.ast/symbol-AST "Qs"))]
    ["1.80 - ( Wj - 1.70 + ( TE / ( qi min ( ( sP / ( ( DM ) - 1.28 ) * 0.45 % 1.00 ) max 1.74 / 0.83 ^ ( ( ( ( 1.09 - ( ( ( ( ( 0.45 + tL + 1.20 ) ) ) + 1.30 * TC / ( ( ( ( Se ) - ( 1.19 ) ) ^ FQ / HI ^ 0.31 ) + 0.94 ) ) % Ih * 1.35 ^ ( 1.59 % 0.40 max ( ( 1.67 max bk ) max ( 0.24 % ( GF + 1.17 % qa ^ 1.28 ) ^ ( NY ^ 0.11 % ( Bg - 1.20 ) ) ) ) ) ) / Ia % ( ( 1.77 min gB / ( ( ( ( ( ov ) / 0.45 / LF - 1.50 ) min 0.57 ) min Tq ^ 0.68 ) ) ) max hd ) ) % 0.34 ^ ( 1.96 ) ) + 0.31 ) ) ) - tQ ^ XB ) ) ) ^ 0.40 + ( ( 0.30 / ( 0.66 max in + 1.90 ) * ( 1.99 / Sl % 1.62 ) / 0.47 ) ^ mc - 1.82 + 0.22 )"
      (rpn.ast/add-AST
        (rpn.ast/subtract-AST
          (rpn.ast/number-AST 1.8)
          (rpn.ast/power-AST
            (rpn.ast/add-AST
              (rpn.ast/subtract-AST
                (rpn.ast/symbol-AST "Wj")
                (rpn.ast/number-AST 1.7))
              (rpn.ast/divide-AST
                (rpn.ast/symbol-AST "TE")
                (rpn.ast/subtract-AST
                  (rpn.ast/min-AST
                    (rpn.ast/symbol-AST "qi")
                    (rpn.ast/power-AST
                      (rpn.ast/divide-AST
                        (rpn.ast/max-AST
                          (rpn.ast/modulo-AST
                            (rpn.ast/multiply-AST
                              (rpn.ast/divide-AST
                                (rpn.ast/symbol-AST "sP")
                                (rpn.ast/subtract-AST
                                  (rpn.ast/symbol-AST "DM")
                                  (rpn.ast/number-AST 1.28)))
                              (rpn.ast/number-AST 0.45))
                            (rpn.ast/number-AST 1.0))
                          (rpn.ast/number-AST 1.74))
                        (rpn.ast/number-AST 0.83))
                      (rpn.ast/add-AST
                        (rpn.ast/power-AST
                          (rpn.ast/modulo-AST
                            (rpn.ast/subtract-AST
                              (rpn.ast/number-AST 1.09)
                              (rpn.ast/modulo-AST
                                (rpn.ast/divide-AST
                                  (rpn.ast/power-AST
                                    (rpn.ast/multiply-AST
                                      (rpn.ast/modulo-AST
                                        (rpn.ast/add-AST
                                          (rpn.ast/add-AST
                                            (rpn.ast/add-AST
                                              (rpn.ast/number-AST 0.45)
                                              (rpn.ast/symbol-AST "tL"))
                                            (rpn.ast/number-AST 1.2))
                                          (rpn.ast/divide-AST
                                            (rpn.ast/multiply-AST
                                              (rpn.ast/number-AST 1.3)
                                              (rpn.ast/symbol-AST "TC"))
                                            (rpn.ast/add-AST
                                              (rpn.ast/power-AST
                                                (rpn.ast/divide-AST
                                                  (rpn.ast/power-AST
                                                    (rpn.ast/subtract-AST
                                                      (rpn.ast/symbol-AST "Se")
                                                      (rpn.ast/number-AST 1.19))
                                                    (rpn.ast/symbol-AST "FQ"))
                                                  (rpn.ast/symbol-AST "HI"))
                                                (rpn.ast/number-AST 0.31))
                                              (rpn.ast/number-AST 0.94))))
                                        (rpn.ast/symbol-AST "Ih"))
                                      (rpn.ast/number-AST 1.35))
                                    (rpn.ast/modulo-AST
                                      (rpn.ast/number-AST 1.59)
                                      (rpn.ast/max-AST
                                        (rpn.ast/number-AST 0.4)
                                        (rpn.ast/max-AST
                                          (rpn.ast/max-AST
                                            (rpn.ast/number-AST 1.67)
                                            (rpn.ast/symbol-AST "bk"))
                                          (rpn.ast/power-AST
                                            (rpn.ast/modulo-AST
                                              (rpn.ast/number-AST 0.24)
                                              (rpn.ast/add-AST
                                                (rpn.ast/symbol-AST "GF")
                                                (rpn.ast/power-AST
                                                  (rpn.ast/modulo-AST
                                                    (rpn.ast/number-AST 1.17)
                                                    (rpn.ast/symbol-AST "qa"))
                                                  (rpn.ast/number-AST 1.28))))
                                            (rpn.ast/modulo-AST
                                              (rpn.ast/power-AST
                                                (rpn.ast/symbol-AST "NY")
                                                (rpn.ast/number-AST 0.11))
                                              (rpn.ast/subtract-AST
                                                (rpn.ast/symbol-AST "Bg")
                                                (rpn.ast/number-AST 1.2))))))))
                                  (rpn.ast/symbol-AST "Ia"))
                                (rpn.ast/max-AST
                                  (rpn.ast/divide-AST
                                    (rpn.ast/min-AST
                                      (rpn.ast/number-AST 1.77)
                                      (rpn.ast/symbol-AST "gB"))
                                    (rpn.ast/power-AST
                                      (rpn.ast/min-AST
                                        (rpn.ast/min-AST
                                          (rpn.ast/subtract-AST
                                            (rpn.ast/divide-AST
                                              (rpn.ast/divide-AST
                                                (rpn.ast/symbol-AST "ov")
                                                (rpn.ast/number-AST 0.45))
                                              (rpn.ast/symbol-AST "LF"))
                                            (rpn.ast/number-AST 1.5))
                                          (rpn.ast/number-AST 0.57))
                                        (rpn.ast/symbol-AST "Tq"))
                                      (rpn.ast/number-AST 0.68)))
                                  (rpn.ast/symbol-AST "hd"))))
                            (rpn.ast/number-AST 0.34))
                          (rpn.ast/number-AST 1.96))
                        (rpn.ast/number-AST 0.31))))
                  (rpn.ast/power-AST
                    (rpn.ast/symbol-AST "tQ")
                    (rpn.ast/symbol-AST "XB")))))
            (rpn.ast/number-AST 0.4)))
        (rpn.ast/add-AST
          (rpn.ast/subtract-AST
            (rpn.ast/power-AST
              (rpn.ast/divide-AST
                (rpn.ast/multiply-AST
                  (rpn.ast/divide-AST
                    (rpn.ast/number-AST 0.3)
                    (rpn.ast/add-AST
                      (rpn.ast/max-AST
                        (rpn.ast/number-AST 0.66)
                        (rpn.ast/symbol-AST "in"))
                      (rpn.ast/number-AST 1.9)))
                  (rpn.ast/modulo-AST
                    (rpn.ast/divide-AST
                      (rpn.ast/number-AST 1.99)
                      (rpn.ast/symbol-AST "Sl"))
                    (rpn.ast/number-AST 1.62)))
                (rpn.ast/number-AST 0.47))
              (rpn.ast/symbol-AST "mc"))
            (rpn.ast/number-AST 1.82))
          (rpn.ast/number-AST 0.22)))]
    ["( 1.23 * HT min 1.16 ) - Oc + 1.44"
      (rpn.ast/add-AST
        (rpn.ast/subtract-AST
          (rpn.ast/multiply-AST
            (rpn.ast/number-AST 1.23)
            (rpn.ast/min-AST
              (rpn.ast/symbol-AST "HT")
              (rpn.ast/number-AST 1.16)))
          (rpn.ast/symbol-AST "Oc"))
        (rpn.ast/number-AST 1.44))]
    ["( 1.99 )"
      (rpn.ast/number-AST 1.99)]
    ["Zu"
      (rpn.ast/symbol-AST "Zu")]
    ["0.53"
      (rpn.ast/number-AST 0.53)]
    ["Nc - PW"
      (rpn.ast/subtract-AST
        (rpn.ast/symbol-AST "Nc")
        (rpn.ast/symbol-AST "PW"))]
    ["kl * ww - YI"
      (rpn.ast/subtract-AST
        (rpn.ast/multiply-AST
          (rpn.ast/symbol-AST "kl")
          (rpn.ast/symbol-AST "ww"))
        (rpn.ast/symbol-AST "YI"))]
    ["1.93 max 1.41 ^ ( ( ( 0.90 % ( 1.40 / 0.79 ^ 0.76 min vt ) ) - ( 0.96 ^ 0.18 ) ) / nl / ( cn % 1.22 ) )"
      (rpn.ast/power-AST
        (rpn.ast/max-AST
          (rpn.ast/number-AST 1.93)
          (rpn.ast/number-AST 1.41))
        (rpn.ast/divide-AST
          (rpn.ast/divide-AST
            (rpn.ast/subtract-AST
              (rpn.ast/modulo-AST
                (rpn.ast/number-AST 0.9)
                (rpn.ast/power-AST
                  (rpn.ast/divide-AST
                    (rpn.ast/number-AST 1.4)
                    (rpn.ast/number-AST 0.79))
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 0.76)
                    (rpn.ast/symbol-AST "vt"))))
              (rpn.ast/power-AST
                (rpn.ast/number-AST 0.96)
                (rpn.ast/number-AST 0.18)))
            (rpn.ast/symbol-AST "nl"))
          (rpn.ast/modulo-AST
            (rpn.ast/symbol-AST "cn")
            (rpn.ast/number-AST 1.22))))]
    ["wo / 0.35 + WT / oj"
      (rpn.ast/add-AST
        (rpn.ast/divide-AST
          (rpn.ast/symbol-AST "wo")
          (rpn.ast/number-AST 0.35))
        (rpn.ast/divide-AST
          (rpn.ast/symbol-AST "WT")
          (rpn.ast/symbol-AST "oj")))]
    ["( 1.42 % mL ) % iF max rI % ( Gb )"
      (rpn.ast/modulo-AST
        (rpn.ast/modulo-AST
          (rpn.ast/modulo-AST
            (rpn.ast/number-AST 1.42)
            (rpn.ast/symbol-AST "mL"))
          (rpn.ast/max-AST
            (rpn.ast/symbol-AST "iF")
            (rpn.ast/symbol-AST "rI")))
        (rpn.ast/symbol-AST "Gb"))]
    ["0.08 ^ KU max 2.00"
      (rpn.ast/power-AST
        (rpn.ast/number-AST 0.08)
        (rpn.ast/max-AST
          (rpn.ast/symbol-AST "KU")
          (rpn.ast/number-AST 2.0)))]
    ["0.33 % 0.51 * 0.42"
      (rpn.ast/multiply-AST
        (rpn.ast/modulo-AST
          (rpn.ast/number-AST 0.33)
          (rpn.ast/number-AST 0.51))
        (rpn.ast/number-AST 0.42))]
    ["rj % rw min ( Et )"
      (rpn.ast/modulo-AST
        (rpn.ast/symbol-AST "rj")
        (rpn.ast/min-AST
          (rpn.ast/symbol-AST "rw")
          (rpn.ast/symbol-AST "Et")))]
    ["( 1.17 ^ 1.70 ) % 0.60 - 1.33"
      (rpn.ast/subtract-AST
        (rpn.ast/modulo-AST
          (rpn.ast/power-AST
            (rpn.ast/number-AST 1.17)
            (rpn.ast/number-AST 1.7))
          (rpn.ast/number-AST 0.6))
        (rpn.ast/number-AST 1.33))]
    ["tp min 0.60"
      (rpn.ast/min-AST
        (rpn.ast/symbol-AST "tp")
        (rpn.ast/number-AST 0.6))]
    ["Zy"
      (rpn.ast/symbol-AST "Zy")]
    ["pT min qW ^ Ul"
      (rpn.ast/power-AST
        (rpn.ast/min-AST
          (rpn.ast/symbol-AST "pT")
          (rpn.ast/symbol-AST "qW"))
        (rpn.ast/symbol-AST "Ul"))]
    ["( 1.36 ^ ET ) ^ Uz"
      (rpn.ast/power-AST
        (rpn.ast/power-AST
          (rpn.ast/number-AST 1.36)
          (rpn.ast/symbol-AST "ET"))
        (rpn.ast/symbol-AST "Uz"))]
    ["( ( 0.16 - 0.44 / LC ) ) ^ vE"
      (rpn.ast/power-AST
        (rpn.ast/subtract-AST
          (rpn.ast/number-AST 0.16)
          (rpn.ast/divide-AST
            (rpn.ast/number-AST 0.44)
            (rpn.ast/symbol-AST "LC")))
        (rpn.ast/symbol-AST "vE"))]
    ["Es min Fl ^ 0.78 ^ 0.60"
      (rpn.ast/power-AST
        (rpn.ast/power-AST
          (rpn.ast/min-AST
            (rpn.ast/symbol-AST "Es")
            (rpn.ast/symbol-AST "Fl"))
          (rpn.ast/number-AST 0.78))
        (rpn.ast/number-AST 0.6))]
    ["1.50"
      (rpn.ast/number-AST 1.5)]
    ["Tn + ( 0.64 ) min 0.62"
      (rpn.ast/add-AST
        (rpn.ast/symbol-AST "Tn")
        (rpn.ast/min-AST
          (rpn.ast/number-AST 0.64)
          (rpn.ast/number-AST 0.62)))]
    ["Pf"
      (rpn.ast/symbol-AST "Pf")]
    ["( ( ( 1.40 ) - ( 1.75 ) max Vq ) * ( UL * 1.74 min ( FP min mV max PQ ) ) / NJ + 1.61 ) / 0.72 % 0.34 max fQ"
      (rpn.ast/modulo-AST
        (rpn.ast/divide-AST
          (rpn.ast/add-AST
            (rpn.ast/divide-AST
              (rpn.ast/multiply-AST
                (rpn.ast/subtract-AST
                  (rpn.ast/number-AST 1.4)
                  (rpn.ast/max-AST
                    (rpn.ast/number-AST 1.75)
                    (rpn.ast/symbol-AST "Vq")))
                (rpn.ast/multiply-AST
                  (rpn.ast/symbol-AST "UL")
                  (rpn.ast/min-AST
                    (rpn.ast/number-AST 1.74)
                    (rpn.ast/max-AST
                      (rpn.ast/min-AST
                        (rpn.ast/symbol-AST "FP")
                        (rpn.ast/symbol-AST "mV"))
                      (rpn.ast/symbol-AST "PQ")))))
              (rpn.ast/symbol-AST "NJ"))
            (rpn.ast/number-AST 1.61))
          (rpn.ast/number-AST 0.72))
        (rpn.ast/max-AST
          (rpn.ast/number-AST 0.34)
          (rpn.ast/symbol-AST "fQ")))]
    ["( 0.42 * ( 0.23 ^ ( fK min bR - ii % ( 1.80 / 1.83 ) ) ) )"
      (rpn.ast/multiply-AST
        (rpn.ast/number-AST 0.42)
        (rpn.ast/power-AST
          (rpn.ast/number-AST 0.23)
          (rpn.ast/subtract-AST
            (rpn.ast/min-AST
              (rpn.ast/symbol-AST "fK")
              (rpn.ast/symbol-AST "bR"))
            (rpn.ast/modulo-AST
              (rpn.ast/symbol-AST "ii")
              (rpn.ast/divide-AST
                (rpn.ast/number-AST 1.8)
                (rpn.ast/number-AST 1.83))))))]
    ["( 1.82 / 1.28 ) max ( Hj - 1.41 )"
      (rpn.ast/max-AST
        (rpn.ast/divide-AST
          (rpn.ast/number-AST 1.82)
          (rpn.ast/number-AST 1.28))
        (rpn.ast/subtract-AST
          (rpn.ast/symbol-AST "Hj")
          (rpn.ast/number-AST 1.41)))]
    ["0.07 % 1.08 * 1.14"
      (rpn.ast/multiply-AST
        (rpn.ast/modulo-AST
          (rpn.ast/number-AST 0.07)
          (rpn.ast/number-AST 1.08))
        (rpn.ast/number-AST 1.14))]
    ["Nt % ( 0.11 ^ LM ) * Pj"
      (rpn.ast/multiply-AST
        (rpn.ast/modulo-AST
          (rpn.ast/symbol-AST "Nt")
          (rpn.ast/power-AST
            (rpn.ast/number-AST 0.11)
            (rpn.ast/symbol-AST "LM")))
        (rpn.ast/symbol-AST "Pj"))]
    ["dD - ( ( 1.93 + ( 1.19 % ZV ^ ( 1.43 * nV * mK ) ) ^ 1.09 ) + ( 0.62 ) % ( ( 1.69 / 1.52 max tJ ) + ( 0.62 ) ) - ( 1.77 / 1.64 max ND min qK ) ) / 1.28 min rN"
      (rpn.ast/subtract-AST
        (rpn.ast/symbol-AST "dD")
        (rpn.ast/divide-AST
          (rpn.ast/subtract-AST
            (rpn.ast/add-AST
              (rpn.ast/add-AST
                (rpn.ast/number-AST 1.93)
                (rpn.ast/power-AST
                  (rpn.ast/power-AST
                    (rpn.ast/modulo-AST
                      (rpn.ast/number-AST 1.19)
                      (rpn.ast/symbol-AST "ZV"))
                    (rpn.ast/multiply-AST
                      (rpn.ast/multiply-AST
                        (rpn.ast/number-AST 1.43)
                        (rpn.ast/symbol-AST "nV"))
                      (rpn.ast/symbol-AST "mK")))
                  (rpn.ast/number-AST 1.09)))
              (rpn.ast/modulo-AST
                (rpn.ast/number-AST 0.62)
                (rpn.ast/add-AST
                  (rpn.ast/divide-AST
                    (rpn.ast/number-AST 1.69)
                    (rpn.ast/max-AST
                      (rpn.ast/number-AST 1.52)
                      (rpn.ast/symbol-AST "tJ")))
                  (rpn.ast/number-AST 0.62))))
            (rpn.ast/divide-AST
              (rpn.ast/number-AST 1.77)
              (rpn.ast/min-AST
                (rpn.ast/max-AST
                  (rpn.ast/number-AST 1.64)
                  (rpn.ast/symbol-AST "ND"))
                (rpn.ast/symbol-AST "qK"))))
          (rpn.ast/min-AST
            (rpn.ast/number-AST 1.28)
            (rpn.ast/symbol-AST "rN"))))]
    ["lu"
      (rpn.ast/symbol-AST "lu")]
    ["( 0.79 )"
      (rpn.ast/number-AST 0.79)]
    ["( ( ( 1.01 ) + 1.63 % Wg / no ) * lY )"
      (rpn.ast/multiply-AST
        (rpn.ast/add-AST
          (rpn.ast/number-AST 1.01)
          (rpn.ast/divide-AST
            (rpn.ast/modulo-AST
              (rpn.ast/number-AST 1.63)
              (rpn.ast/symbol-AST "Wg"))
            (rpn.ast/symbol-AST "no")))
        (rpn.ast/symbol-AST "lY"))]
    ["( ( Rq ) ) ^ 1.64"
      (rpn.ast/power-AST
        (rpn.ast/symbol-AST "Rq")
        (rpn.ast/number-AST 1.64))]
    ["1.61"
      (rpn.ast/number-AST 1.61)]
    ["1.74 * 0.16 * ( 1.96 ^ 0.58 % 0.85 * oc ) / ( 0.52 )"
      (rpn.ast/divide-AST
        (rpn.ast/multiply-AST
          (rpn.ast/multiply-AST
            (rpn.ast/number-AST 1.74)
            (rpn.ast/number-AST 0.16))
          (rpn.ast/multiply-AST
            (rpn.ast/modulo-AST
              (rpn.ast/power-AST
                (rpn.ast/number-AST 1.96)
                (rpn.ast/number-AST 0.58))
              (rpn.ast/number-AST 0.85))
            (rpn.ast/symbol-AST "oc")))
        (rpn.ast/number-AST 0.52))]
    ["0.22"
      (rpn.ast/number-AST 0.22)]))
