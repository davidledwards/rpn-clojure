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
(ns rpn.generator-test
  (:require [clojure.test :as test])
  (:require [rpn.lexer :as lexer])
  (:require [rpn.parser :as parser])
  (:require [rpn.generator :as gen])
  (:require [rpn.code :as code]))

(declare generator-tests)

(test/deftest valid-expressions
  (doseq [[e cs] generator-tests]
    (let [codes (gen/generator (parser/parser (lexer/lexer e)))]
      (test/is (= (count codes) (count cs)))
      (for [[x y] (map vector codes cs)]
        (test/is (= (code/instruction x) (code/instruction y)))))))

(def ^:private generator-tests
  (list
    ["( 0.92 min jP ) * 0.08 % ( 1.16 )"
      (list
        (rpn.code/declare-symbol-code "jP")
        (rpn.code/push-code 0.92)
        (rpn.code/push-symbol-code "jP")
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.08)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.16)
        rpn.code/modulo-code)]
    ["( iC - ( 0.50 max yq ) min 1.61 % 1.58 )"
      (list
        (rpn.code/declare-symbol-code "iC")
        (rpn.code/declare-symbol-code "yq")
        (rpn.code/push-symbol-code "iC")
        (rpn.code/push-code 0.5)
        (rpn.code/push-symbol-code "yq")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.61)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.58)
        rpn.code/modulo-code
        (rpn.code/subtract-code 2))]
    ["1.95 ^ 0.38 min ( KX / ( 1.26 - 1.40 ^ 0.04 ^ Ir ) + 1.25 ) + ( ( ( ( 1.30 + 1.61 ) + aS % Kh max ( Xq + ( 0.48 ) + ( Ld ) min 0.43 ) ) - mE ) * Uw * ( Yw % ( 0.18 max 1.21 ) + Pb ) )"
      (list
        (rpn.code/declare-symbol-code "Ir")
        (rpn.code/declare-symbol-code "KX")
        (rpn.code/declare-symbol-code "Kh")
        (rpn.code/declare-symbol-code "Ld")
        (rpn.code/declare-symbol-code "Pb")
        (rpn.code/declare-symbol-code "Uw")
        (rpn.code/declare-symbol-code "Xq")
        (rpn.code/declare-symbol-code "Yw")
        (rpn.code/declare-symbol-code "aS")
        (rpn.code/declare-symbol-code "mE")
        (rpn.code/push-code 1.95)
        (rpn.code/push-code 0.38)
        (rpn.code/push-symbol-code "KX")
        (rpn.code/push-code 1.26)
        (rpn.code/push-code 1.4)
        (rpn.code/push-code 0.04)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Ir")
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.25)
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        rpn.code/power-code
        (rpn.code/push-code 1.3)
        (rpn.code/push-code 1.61)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "aS")
        (rpn.code/push-symbol-code "Kh")
        (rpn.code/push-symbol-code "Xq")
        (rpn.code/push-code 0.48)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "Ld")
        (rpn.code/push-code 0.43)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        (rpn.code/max-code 2)
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "mE")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Uw")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "Yw")
        (rpn.code/push-code 0.18)
        (rpn.code/push-code 1.21)
        (rpn.code/max-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "Pb")
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2))]
    ["( ( ( 0.48 max ( 0.14 ) ) % ( 0.88 / 0.45 max 1.60 ) ) - 1.71 max ( ( 1.73 ) ) max ( ( QU max Lo * 0.64 ) min 1.03 + 0.57 + 1.40 ) ) min 0.73 - Po - GN"
      (list
        (rpn.code/declare-symbol-code "GN")
        (rpn.code/declare-symbol-code "Lo")
        (rpn.code/declare-symbol-code "Po")
        (rpn.code/declare-symbol-code "QU")
        (rpn.code/push-code 0.48)
        (rpn.code/push-code 0.14)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.88)
        (rpn.code/push-code 0.45)
        (rpn.code/push-code 1.6)
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 1.71)
        (rpn.code/push-code 1.73)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "QU")
        (rpn.code/push-symbol-code "Lo")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.64)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.03)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.57)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.4)
        (rpn.code/add-code 2)
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.73)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "Po")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "GN")
        (rpn.code/subtract-code 2))]
    ["( 0.78 min WP ) * ( 0.61 min ( 0.77 - ( ( lB + YX / DV ) * ZW ^ lv * 0.29 ) - 0.62 ) + 1.59 / 0.56 ) ^ jS"
      (list
        (rpn.code/declare-symbol-code "DV")
        (rpn.code/declare-symbol-code "WP")
        (rpn.code/declare-symbol-code "YX")
        (rpn.code/declare-symbol-code "ZW")
        (rpn.code/declare-symbol-code "jS")
        (rpn.code/declare-symbol-code "lB")
        (rpn.code/declare-symbol-code "lv")
        (rpn.code/push-code 0.78)
        (rpn.code/push-symbol-code "WP")
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.61)
        (rpn.code/push-code 0.77)
        (rpn.code/push-symbol-code "lB")
        (rpn.code/push-symbol-code "YX")
        (rpn.code/push-symbol-code "DV")
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "ZW")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "lv")
        rpn.code/power-code
        (rpn.code/push-code 0.29)
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.62)
        (rpn.code/subtract-code 2)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.59)
        (rpn.code/push-code 0.56)
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "jS")
        rpn.code/power-code)]
    ["( aJ - Ky - ( yi - ( 0.59 ^ ( xi max dP / ( ( Jx min ( ( ( 1.86 / ( ( 0.60 max NG ) / 0.69 / ( 1.11 ^ 0.88 ^ ( Xh ) - 0.59 ) ) * 0.68 min ( ( ( Hq ) min 1.55 ) ^ 1.24 % ( 1.54 ) min Ez ) ) * 1.46 % 1.01 * JP ) max 0.43 max 0.77 max ( 0.55 - ( kN ) / 1.54 ) ) ) + ul * ( ( 1.77 ^ 1.54 / uj ) - do / vU % 1.72 ) ) ) ) + 0.10 min ( ( 0.40 ^ ( Md max zz - 1.73 max 1.99 ) max ( ( 1.28 max Nw min ( ( KN / qG + ( UT ) * 0.47 ) % eD ) ) max Tv ) - Hy ) ) ) max 1.62 ) / ( 1.02 )"
      (list
        (rpn.code/declare-symbol-code "Ez")
        (rpn.code/declare-symbol-code "Hq")
        (rpn.code/declare-symbol-code "Hy")
        (rpn.code/declare-symbol-code "JP")
        (rpn.code/declare-symbol-code "Jx")
        (rpn.code/declare-symbol-code "KN")
        (rpn.code/declare-symbol-code "Ky")
        (rpn.code/declare-symbol-code "Md")
        (rpn.code/declare-symbol-code "NG")
        (rpn.code/declare-symbol-code "Nw")
        (rpn.code/declare-symbol-code "Tv")
        (rpn.code/declare-symbol-code "UT")
        (rpn.code/declare-symbol-code "Xh")
        (rpn.code/declare-symbol-code "aJ")
        (rpn.code/declare-symbol-code "dP")
        (rpn.code/declare-symbol-code "do")
        (rpn.code/declare-symbol-code "eD")
        (rpn.code/declare-symbol-code "kN")
        (rpn.code/declare-symbol-code "qG")
        (rpn.code/declare-symbol-code "uj")
        (rpn.code/declare-symbol-code "ul")
        (rpn.code/declare-symbol-code "vU")
        (rpn.code/declare-symbol-code "xi")
        (rpn.code/declare-symbol-code "yi")
        (rpn.code/declare-symbol-code "zz")
        (rpn.code/push-symbol-code "aJ")
        (rpn.code/push-symbol-code "Ky")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "yi")
        (rpn.code/push-code 0.59)
        (rpn.code/push-symbol-code "xi")
        (rpn.code/push-symbol-code "dP")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "Jx")
        (rpn.code/push-code 1.86)
        (rpn.code/push-code 0.6)
        (rpn.code/push-symbol-code "NG")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.69)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.11)
        (rpn.code/push-code 0.88)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Xh")
        rpn.code/power-code
        (rpn.code/push-code 0.59)
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.68)
        (rpn.code/push-symbol-code "Hq")
        (rpn.code/push-code 1.55)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.24)
        rpn.code/power-code
        (rpn.code/push-code 1.54)
        (rpn.code/push-symbol-code "Ez")
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/min-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.46)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.01)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "JP")
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.43)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.77)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.55)
        (rpn.code/push-symbol-code "kN")
        (rpn.code/push-code 1.54)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/max-code 2)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "ul")
        (rpn.code/push-code 1.77)
        (rpn.code/push-code 1.54)
        rpn.code/power-code
        (rpn.code/push-symbol-code "uj")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "do")
        (rpn.code/push-symbol-code "vU")
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.72)
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/divide-code 2)
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.1)
        (rpn.code/push-code 0.4)
        (rpn.code/push-symbol-code "Md")
        (rpn.code/push-symbol-code "zz")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.73)
        (rpn.code/push-code 1.99)
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.28)
        (rpn.code/push-symbol-code "Nw")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "KN")
        (rpn.code/push-symbol-code "qG")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "UT")
        (rpn.code/push-code 0.47)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "eD")
        rpn.code/modulo-code
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "Tv")
        (rpn.code/max-code 2)
        (rpn.code/max-code 2)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Hy")
        (rpn.code/subtract-code 2)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.62)
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.02)
        (rpn.code/divide-code 2))]
    ["zm + FE min Xn"
      (list
        (rpn.code/declare-symbol-code "FE")
        (rpn.code/declare-symbol-code "Xn")
        (rpn.code/declare-symbol-code "zm")
        (rpn.code/push-symbol-code "zm")
        (rpn.code/push-symbol-code "FE")
        (rpn.code/push-symbol-code "Xn")
        (rpn.code/min-code 2)
        (rpn.code/add-code 2))]
    ["( 0.93 / Fe * ( XJ max bI - ( ( 1.70 ) / 0.81 ) % ( EA - 1.18 ) ) * 0.38 ) min ( 0.05 ) max ( 0.95 ) ^ QO"
      (list
        (rpn.code/declare-symbol-code "EA")
        (rpn.code/declare-symbol-code "Fe")
        (rpn.code/declare-symbol-code "QO")
        (rpn.code/declare-symbol-code "XJ")
        (rpn.code/declare-symbol-code "bI")
        (rpn.code/push-code 0.93)
        (rpn.code/push-symbol-code "Fe")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "XJ")
        (rpn.code/push-symbol-code "bI")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.7)
        (rpn.code/push-code 0.81)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "EA")
        (rpn.code/push-code 1.18)
        (rpn.code/subtract-code 2)
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.38)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.05)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.95)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "QO")
        rpn.code/power-code)]
    ["1.49 - HE min Og"
      (list
        (rpn.code/declare-symbol-code "HE")
        (rpn.code/declare-symbol-code "Og")
        (rpn.code/push-code 1.49)
        (rpn.code/push-symbol-code "HE")
        (rpn.code/push-symbol-code "Og")
        (rpn.code/min-code 2)
        (rpn.code/subtract-code 2))]
    ["( ( 0.13 ) * 1.67 - GD % ( 0.65 / 1.54 ^ 0.29 ) ) % ( qA ) % WS"
      (list
        (rpn.code/declare-symbol-code "GD")
        (rpn.code/declare-symbol-code "WS")
        (rpn.code/declare-symbol-code "qA")
        (rpn.code/push-code 0.13)
        (rpn.code/push-code 1.67)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "GD")
        (rpn.code/push-code 0.65)
        (rpn.code/push-code 1.54)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.29)
        rpn.code/power-code
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "qA")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "WS")
        rpn.code/modulo-code)]
    ["( 1.59 min JW ) + MK min ey max ( ( GX / 0.56 / ( ( MR % ( 1.26 % 0.73 ) - ( ( 1.94 ^ 0.02 - 0.98 ) ) ) min mr - KY ) ) min zV * NU max Ic )"
      (list
        (rpn.code/declare-symbol-code "GX")
        (rpn.code/declare-symbol-code "Ic")
        (rpn.code/declare-symbol-code "JW")
        (rpn.code/declare-symbol-code "KY")
        (rpn.code/declare-symbol-code "MK")
        (rpn.code/declare-symbol-code "MR")
        (rpn.code/declare-symbol-code "NU")
        (rpn.code/declare-symbol-code "ey")
        (rpn.code/declare-symbol-code "mr")
        (rpn.code/declare-symbol-code "zV")
        (rpn.code/push-code 1.59)
        (rpn.code/push-symbol-code "JW")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "MK")
        (rpn.code/push-symbol-code "ey")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "GX")
        (rpn.code/push-code 0.56)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "MR")
        (rpn.code/push-code 1.26)
        (rpn.code/push-code 0.73)
        rpn.code/modulo-code
        rpn.code/modulo-code
        (rpn.code/push-code 1.94)
        (rpn.code/push-code 0.02)
        rpn.code/power-code
        (rpn.code/push-code 0.98)
        (rpn.code/subtract-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "mr")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "KY")
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "zV")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "NU")
        (rpn.code/push-symbol-code "Ic")
        (rpn.code/max-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/max-code 2)
        (rpn.code/add-code 2))]
    ["Tv % ( ( JL ) ) * iv"
      (list
        (rpn.code/declare-symbol-code "JL")
        (rpn.code/declare-symbol-code "Tv")
        (rpn.code/declare-symbol-code "iv")
        (rpn.code/push-symbol-code "Tv")
        (rpn.code/push-symbol-code "JL")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "iv")
        (rpn.code/multiply-code 2))]
    ["Mu"
      (list
        (rpn.code/declare-symbol-code "Mu")
        (rpn.code/push-symbol-code "Mu"))]
    ["cJ min rB * ( ( qb % ( 0.71 * ( ( ( ( SS ) / tZ max 1.94 ) max 0.77 ^ 0.99 - ( ( ( CD ) - tg ) - HO ) ) / Rt ) % 1.23 ) ) - 0.43 max fV - ( ( 1.89 ) + Zu - oX ) )"
      (list
        (rpn.code/declare-symbol-code "CD")
        (rpn.code/declare-symbol-code "HO")
        (rpn.code/declare-symbol-code "Rt")
        (rpn.code/declare-symbol-code "SS")
        (rpn.code/declare-symbol-code "Zu")
        (rpn.code/declare-symbol-code "cJ")
        (rpn.code/declare-symbol-code "fV")
        (rpn.code/declare-symbol-code "oX")
        (rpn.code/declare-symbol-code "qb")
        (rpn.code/declare-symbol-code "rB")
        (rpn.code/declare-symbol-code "tZ")
        (rpn.code/declare-symbol-code "tg")
        (rpn.code/push-symbol-code "cJ")
        (rpn.code/push-symbol-code "rB")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "qb")
        (rpn.code/push-code 0.71)
        (rpn.code/push-symbol-code "SS")
        (rpn.code/push-symbol-code "tZ")
        (rpn.code/push-code 1.94)
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.77)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.99)
        rpn.code/power-code
        (rpn.code/push-symbol-code "CD")
        (rpn.code/push-symbol-code "tg")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "HO")
        (rpn.code/subtract-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Rt")
        (rpn.code/divide-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.23)
        rpn.code/modulo-code
        rpn.code/modulo-code
        (rpn.code/push-code 0.43)
        (rpn.code/push-symbol-code "fV")
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.89)
        (rpn.code/push-symbol-code "Zu")
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "oX")
        (rpn.code/subtract-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2))]
    ["( ( ro ^ 1.06 max pR ^ rV ) / ( Kj ) + ma / 1.30 )"
      (list
        (rpn.code/declare-symbol-code "Kj")
        (rpn.code/declare-symbol-code "ma")
        (rpn.code/declare-symbol-code "pR")
        (rpn.code/declare-symbol-code "rV")
        (rpn.code/declare-symbol-code "ro")
        (rpn.code/push-symbol-code "ro")
        (rpn.code/push-code 1.06)
        (rpn.code/push-symbol-code "pR")
        (rpn.code/max-code 2)
        rpn.code/power-code
        (rpn.code/push-symbol-code "rV")
        rpn.code/power-code
        (rpn.code/push-symbol-code "Kj")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "ma")
        (rpn.code/push-code 1.3)
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2))]
    ["( ld )"
      (list
        (rpn.code/declare-symbol-code "ld")
        (rpn.code/push-symbol-code "ld"))]
    ["zz * 1.83"
      (list
        (rpn.code/declare-symbol-code "zz")
        (rpn.code/push-symbol-code "zz")
        (rpn.code/push-code 1.83)
        (rpn.code/multiply-code 2))]
    ["il max ( ( zQ ^ uK - ( nl % ( 0.27 ) ) max 1.63 ) ) min ec"
      (list
        (rpn.code/declare-symbol-code "ec")
        (rpn.code/declare-symbol-code "il")
        (rpn.code/declare-symbol-code "nl")
        (rpn.code/declare-symbol-code "uK")
        (rpn.code/declare-symbol-code "zQ")
        (rpn.code/push-symbol-code "il")
        (rpn.code/push-symbol-code "zQ")
        (rpn.code/push-symbol-code "uK")
        rpn.code/power-code
        (rpn.code/push-symbol-code "nl")
        (rpn.code/push-code 0.27)
        rpn.code/modulo-code
        (rpn.code/push-code 1.63)
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "ec")
        (rpn.code/min-code 2))]
    ["( GQ / yD * Zy ) * 0.35 + cj max lf"
      (list
        (rpn.code/declare-symbol-code "GQ")
        (rpn.code/declare-symbol-code "Zy")
        (rpn.code/declare-symbol-code "cj")
        (rpn.code/declare-symbol-code "lf")
        (rpn.code/declare-symbol-code "yD")
        (rpn.code/push-symbol-code "GQ")
        (rpn.code/push-symbol-code "yD")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Zy")
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.35)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "cj")
        (rpn.code/push-symbol-code "lf")
        (rpn.code/max-code 2)
        (rpn.code/add-code 2))]
    ["( ( Ij / 1.94 + lk ) min 1.33 % ( ( ( ( oh + ( ( ( 0.68 ) + xt min ( PC / ( 1.65 + ( ( Vi ) * Yi - XK + ( 0.78 / ( Dq - 1.26 min 0.17 min 1.11 ) - ( ( TU / ( ( uj ^ fv ) ) / zG ) ^ 0.22 / 1.90 ^ ( 0.96 / Zf + yH ) ) - ( ( Sw % 0.68 * ( 0.91 ^ uV ) ) - oV / Sq % 0.04 ) ) ) - ( ( 0.27 * Zk ^ 1.91 ) ) ) + BU / 1.32 ) ) ) * ( ( 1.63 max 1.59 min ( mA ^ ( zF / ( 0.17 ) ) ) min 1.93 ) - ( ( dF + ( 0.27 - EF ) ^ 1.74 + ( 1.63 % eo ) ) ) * 0.26 max 0.92 ) ) - ( 1.89 ^ 0.45 min 1.19 ) min 1.87 / ( ( Mg ^ ( qJ - 1.17 + ( ( Sp ^ uf / ( ( 1.19 % ( ( 1.24 min 0.45 min Av + ( yV % ( 0.53 ) * YG ) ) % ( pU + uS / 0.33 ) + ( ( ( ( ( CL - ( ( pQ max 1.22 / Xf ) max ey / 0.22 - ( 1.11 ^ 0.81 ) ) ) % zr % ( 1.08 ) * ( ( 0.82 ^ Lj / 0.63 - Pi ) % kO ) ) max ( 1.77 - 1.58 - ( ( sC min 1.53 ) min ( 0.14 / Pf min 0.99 ^ ED ) ) % ( 1.97 ) ) ) ) / 0.34 + 1.65 ) ) ^ 0.76 - 1.26 ) ) ) / aB ) ) ) / 0.01 * 1.53 ) ) max ( 1.19 ) min oW ) max ( ( XO ) max 0.06 min 0.98 min aS ) ) )"
      (list
        (rpn.code/declare-symbol-code "Av")
        (rpn.code/declare-symbol-code "BU")
        (rpn.code/declare-symbol-code "CL")
        (rpn.code/declare-symbol-code "Dq")
        (rpn.code/declare-symbol-code "ED")
        (rpn.code/declare-symbol-code "EF")
        (rpn.code/declare-symbol-code "Ij")
        (rpn.code/declare-symbol-code "Lj")
        (rpn.code/declare-symbol-code "Mg")
        (rpn.code/declare-symbol-code "PC")
        (rpn.code/declare-symbol-code "Pf")
        (rpn.code/declare-symbol-code "Pi")
        (rpn.code/declare-symbol-code "Sp")
        (rpn.code/declare-symbol-code "Sq")
        (rpn.code/declare-symbol-code "Sw")
        (rpn.code/declare-symbol-code "TU")
        (rpn.code/declare-symbol-code "Vi")
        (rpn.code/declare-symbol-code "XK")
        (rpn.code/declare-symbol-code "XO")
        (rpn.code/declare-symbol-code "Xf")
        (rpn.code/declare-symbol-code "YG")
        (rpn.code/declare-symbol-code "Yi")
        (rpn.code/declare-symbol-code "Zf")
        (rpn.code/declare-symbol-code "Zk")
        (rpn.code/declare-symbol-code "aB")
        (rpn.code/declare-symbol-code "aS")
        (rpn.code/declare-symbol-code "dF")
        (rpn.code/declare-symbol-code "eo")
        (rpn.code/declare-symbol-code "ey")
        (rpn.code/declare-symbol-code "fv")
        (rpn.code/declare-symbol-code "kO")
        (rpn.code/declare-symbol-code "lk")
        (rpn.code/declare-symbol-code "mA")
        (rpn.code/declare-symbol-code "oV")
        (rpn.code/declare-symbol-code "oW")
        (rpn.code/declare-symbol-code "oh")
        (rpn.code/declare-symbol-code "pQ")
        (rpn.code/declare-symbol-code "pU")
        (rpn.code/declare-symbol-code "qJ")
        (rpn.code/declare-symbol-code "sC")
        (rpn.code/declare-symbol-code "uS")
        (rpn.code/declare-symbol-code "uV")
        (rpn.code/declare-symbol-code "uf")
        (rpn.code/declare-symbol-code "uj")
        (rpn.code/declare-symbol-code "xt")
        (rpn.code/declare-symbol-code "yH")
        (rpn.code/declare-symbol-code "yV")
        (rpn.code/declare-symbol-code "zF")
        (rpn.code/declare-symbol-code "zG")
        (rpn.code/declare-symbol-code "zr")
        (rpn.code/push-symbol-code "Ij")
        (rpn.code/push-code 1.94)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "lk")
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.33)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "oh")
        (rpn.code/push-code 0.68)
        (rpn.code/push-symbol-code "xt")
        (rpn.code/push-symbol-code "PC")
        (rpn.code/push-code 1.65)
        (rpn.code/push-symbol-code "Vi")
        (rpn.code/push-symbol-code "Yi")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "XK")
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.78)
        (rpn.code/push-symbol-code "Dq")
        (rpn.code/push-code 1.26)
        (rpn.code/push-code 0.17)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.11)
        (rpn.code/min-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "TU")
        (rpn.code/push-symbol-code "uj")
        (rpn.code/push-symbol-code "fv")
        rpn.code/power-code
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "zG")
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.22)
        rpn.code/power-code
        (rpn.code/push-code 1.9)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.96)
        (rpn.code/push-symbol-code "Zf")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "yH")
        (rpn.code/add-code 2)
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Sw")
        (rpn.code/push-code 0.68)
        rpn.code/modulo-code
        (rpn.code/push-code 0.91)
        (rpn.code/push-symbol-code "uV")
        rpn.code/power-code
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "oV")
        (rpn.code/push-symbol-code "Sq")
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.04)
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/add-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.27)
        (rpn.code/push-symbol-code "Zk")
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.91)
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "BU")
        (rpn.code/push-code 1.32)
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.63)
        (rpn.code/push-code 1.59)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "mA")
        (rpn.code/push-symbol-code "zF")
        (rpn.code/push-code 0.17)
        (rpn.code/divide-code 2)
        rpn.code/power-code
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.93)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "dF")
        (rpn.code/push-code 0.27)
        (rpn.code/push-symbol-code "EF")
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.74)
        rpn.code/power-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.63)
        (rpn.code/push-symbol-code "eo")
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.26)
        (rpn.code/push-code 0.92)
        (rpn.code/max-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.89)
        (rpn.code/push-code 0.45)
        (rpn.code/push-code 1.19)
        (rpn.code/min-code 2)
        rpn.code/power-code
        (rpn.code/push-code 1.87)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "Mg")
        (rpn.code/push-symbol-code "qJ")
        (rpn.code/push-code 1.17)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Sp")
        (rpn.code/push-symbol-code "uf")
        rpn.code/power-code
        (rpn.code/push-code 1.19)
        (rpn.code/push-code 1.24)
        (rpn.code/push-code 0.45)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "Av")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "yV")
        (rpn.code/push-code 0.53)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "YG")
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "pU")
        (rpn.code/push-symbol-code "uS")
        (rpn.code/push-code 0.33)
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "CL")
        (rpn.code/push-symbol-code "pQ")
        (rpn.code/push-code 1.22)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "Xf")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "ey")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.22)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.11)
        (rpn.code/push-code 0.81)
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "zr")
        rpn.code/modulo-code
        (rpn.code/push-code 1.08)
        rpn.code/modulo-code
        (rpn.code/push-code 0.82)
        (rpn.code/push-symbol-code "Lj")
        rpn.code/power-code
        (rpn.code/push-code 0.63)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Pi")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "kO")
        rpn.code/modulo-code
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.77)
        (rpn.code/push-code 1.58)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "sC")
        (rpn.code/push-code 1.53)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.14)
        (rpn.code/push-symbol-code "Pf")
        (rpn.code/push-code 0.99)
        (rpn.code/min-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "ED")
        rpn.code/power-code
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.97)
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.34)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.65)
        (rpn.code/add-code 2)
        (rpn.code/add-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 0.76)
        rpn.code/power-code
        (rpn.code/push-code 1.26)
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "aB")
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        rpn.code/power-code
        (rpn.code/push-code 0.01)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.53)
        (rpn.code/multiply-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.19)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "oW")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "XO")
        (rpn.code/push-code 0.06)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.98)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "aS")
        (rpn.code/min-code 2)
        (rpn.code/max-code 2)
        rpn.code/modulo-code)]
    ["0.81 - 0.14"
      (list
        (rpn.code/push-code 0.81)
        (rpn.code/push-code 0.14)
        (rpn.code/subtract-code 2))]
    ["( 1.02 % mk ) min TH / 1.53"
      (list
        (rpn.code/declare-symbol-code "TH")
        (rpn.code/declare-symbol-code "mk")
        (rpn.code/push-code 1.02)
        (rpn.code/push-symbol-code "mk")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "TH")
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.53)
        (rpn.code/divide-code 2))]
    ["0.25"
      (list
        (rpn.code/push-code 0.25))]
    ["PL % iv % 0.26 + ( 1.05 )"
      (list
        (rpn.code/declare-symbol-code "PL")
        (rpn.code/declare-symbol-code "iv")
        (rpn.code/push-symbol-code "PL")
        (rpn.code/push-symbol-code "iv")
        rpn.code/modulo-code
        (rpn.code/push-code 0.26)
        rpn.code/modulo-code
        (rpn.code/push-code 1.05)
        (rpn.code/add-code 2))]
    ["1.80 + ( oz - 1.16 - mh ) ^ 1.43 * ( Wh )"
      (list
        (rpn.code/declare-symbol-code "Wh")
        (rpn.code/declare-symbol-code "mh")
        (rpn.code/declare-symbol-code "oz")
        (rpn.code/push-code 1.8)
        (rpn.code/push-symbol-code "oz")
        (rpn.code/push-code 1.16)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "mh")
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.43)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Wh")
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2))]
    ["( 0.49 ) - 0.65 / 0.27"
      (list
        (rpn.code/push-code 0.49)
        (rpn.code/push-code 0.65)
        (rpn.code/push-code 0.27)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2))]
    ["vt"
      (list
        (rpn.code/declare-symbol-code "vt")
        (rpn.code/push-symbol-code "vt"))]
    ["( ( 0.66 min 0.66 min 1.60 ^ ( KM - AN / ( ( 0.96 + ed * ( ( ql + ( xB ) ) ) ) ) ) ) ) / VM / ( Ch * 0.83 ) - db"
      (list
        (rpn.code/declare-symbol-code "AN")
        (rpn.code/declare-symbol-code "Ch")
        (rpn.code/declare-symbol-code "KM")
        (rpn.code/declare-symbol-code "VM")
        (rpn.code/declare-symbol-code "db")
        (rpn.code/declare-symbol-code "ed")
        (rpn.code/declare-symbol-code "ql")
        (rpn.code/declare-symbol-code "xB")
        (rpn.code/push-code 0.66)
        (rpn.code/push-code 0.66)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.6)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "KM")
        (rpn.code/push-symbol-code "AN")
        (rpn.code/push-code 0.96)
        (rpn.code/push-symbol-code "ed")
        (rpn.code/push-symbol-code "ql")
        (rpn.code/push-symbol-code "xB")
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2)
        rpn.code/power-code
        (rpn.code/push-symbol-code "VM")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Ch")
        (rpn.code/push-code 0.83)
        (rpn.code/multiply-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "db")
        (rpn.code/subtract-code 2))]
    ["1.38 + 1.24"
      (list
        (rpn.code/push-code 1.38)
        (rpn.code/push-code 1.24)
        (rpn.code/add-code 2))]
    ["ZT"
      (list
        (rpn.code/declare-symbol-code "ZT")
        (rpn.code/push-symbol-code "ZT"))]
    ["NL"
      (list
        (rpn.code/declare-symbol-code "NL")
        (rpn.code/push-symbol-code "NL"))]
    ["1.36 max ( ( yi / ( 1.00 % Mp min Iu + vA ) * ( qa - ( Ag - ( 0.89 % IJ ) + yh ) + TA ) / Ni ) ) - ( 0.33 - ( nQ max 1.40 * ( ( ( ( we max ( da % 0.49 % 1.94 min ( 1.39 * ( ( jw min 0.84 ) max ( 0.58 min RT ) - ( ( fJ / ( fs max XZ ) ) ) ) * 1.43 ) ) - ( 1.92 * 1.46 + 0.81 ) ) ) min 0.13 ) ^ 0.29 ) max ( 0.17 ^ 1.75 % ( hH min 0.36 ) ) ) min 0.86 ) ^ 1.13"
      (list
        (rpn.code/declare-symbol-code "Ag")
        (rpn.code/declare-symbol-code "IJ")
        (rpn.code/declare-symbol-code "Iu")
        (rpn.code/declare-symbol-code "Mp")
        (rpn.code/declare-symbol-code "Ni")
        (rpn.code/declare-symbol-code "RT")
        (rpn.code/declare-symbol-code "TA")
        (rpn.code/declare-symbol-code "XZ")
        (rpn.code/declare-symbol-code "da")
        (rpn.code/declare-symbol-code "fJ")
        (rpn.code/declare-symbol-code "fs")
        (rpn.code/declare-symbol-code "hH")
        (rpn.code/declare-symbol-code "jw")
        (rpn.code/declare-symbol-code "nQ")
        (rpn.code/declare-symbol-code "qa")
        (rpn.code/declare-symbol-code "vA")
        (rpn.code/declare-symbol-code "we")
        (rpn.code/declare-symbol-code "yh")
        (rpn.code/declare-symbol-code "yi")
        (rpn.code/push-code 1.36)
        (rpn.code/push-symbol-code "yi")
        (rpn.code/push-code 1.0)
        (rpn.code/push-symbol-code "Mp")
        (rpn.code/push-symbol-code "Iu")
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "vA")
        (rpn.code/add-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "qa")
        (rpn.code/push-symbol-code "Ag")
        (rpn.code/push-code 0.89)
        (rpn.code/push-symbol-code "IJ")
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "yh")
        (rpn.code/add-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "TA")
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "Ni")
        (rpn.code/divide-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.33)
        (rpn.code/push-symbol-code "nQ")
        (rpn.code/push-code 1.4)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "we")
        (rpn.code/push-symbol-code "da")
        (rpn.code/push-code 0.49)
        rpn.code/modulo-code
        (rpn.code/push-code 1.94)
        (rpn.code/push-code 1.39)
        (rpn.code/push-symbol-code "jw")
        (rpn.code/push-code 0.84)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.58)
        (rpn.code/push-symbol-code "RT")
        (rpn.code/min-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "fJ")
        (rpn.code/push-symbol-code "fs")
        (rpn.code/push-symbol-code "XZ")
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.43)
        (rpn.code/multiply-code 2)
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.92)
        (rpn.code/push-code 1.46)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.81)
        (rpn.code/add-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.13)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.29)
        rpn.code/power-code
        (rpn.code/push-code 0.17)
        (rpn.code/push-code 1.75)
        rpn.code/power-code
        (rpn.code/push-symbol-code "hH")
        (rpn.code/push-code 0.36)
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/max-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.86)
        (rpn.code/min-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.13)
        rpn.code/power-code
        (rpn.code/subtract-code 2))]
    ["( hr )"
      (list
        (rpn.code/declare-symbol-code "hr")
        (rpn.code/push-symbol-code "hr"))]
    ["aE - tQ"
      (list
        (rpn.code/declare-symbol-code "aE")
        (rpn.code/declare-symbol-code "tQ")
        (rpn.code/push-symbol-code "aE")
        (rpn.code/push-symbol-code "tQ")
        (rpn.code/subtract-code 2))]
    ["bu"
      (list
        (rpn.code/declare-symbol-code "bu")
        (rpn.code/push-symbol-code "bu"))]
    ["hU max ( jm * 0.52 / ( 1.24 - ( LM ) min ( WR ) ^ rD ) max Ci ) + ( ( 0.54 max oR ^ cj ^ 1.17 ) + ( 1.53 / br + 0.26 ) )"
      (list
        (rpn.code/declare-symbol-code "Ci")
        (rpn.code/declare-symbol-code "LM")
        (rpn.code/declare-symbol-code "WR")
        (rpn.code/declare-symbol-code "br")
        (rpn.code/declare-symbol-code "cj")
        (rpn.code/declare-symbol-code "hU")
        (rpn.code/declare-symbol-code "jm")
        (rpn.code/declare-symbol-code "oR")
        (rpn.code/declare-symbol-code "rD")
        (rpn.code/push-symbol-code "hU")
        (rpn.code/push-symbol-code "jm")
        (rpn.code/push-code 0.52)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.24)
        (rpn.code/push-symbol-code "LM")
        (rpn.code/push-symbol-code "WR")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "rD")
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Ci")
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.54)
        (rpn.code/push-symbol-code "oR")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "cj")
        rpn.code/power-code
        (rpn.code/push-code 1.17)
        rpn.code/power-code
        (rpn.code/push-code 1.53)
        (rpn.code/push-symbol-code "br")
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.26)
        (rpn.code/add-code 2)
        (rpn.code/add-code 2)
        (rpn.code/add-code 2))]
    ["( 1.18 + JC min 0.92 ) / 2.00"
      (list
        (rpn.code/declare-symbol-code "JC")
        (rpn.code/push-code 1.18)
        (rpn.code/push-symbol-code "JC")
        (rpn.code/push-code 0.92)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 2.0)
        (rpn.code/divide-code 2))]
    ["( jC max aA * ( 1.16 + 1.36 max 1.78 ) * ( 1.60 max 0.13 - 0.77 ) ) ^ 1.09 ^ 1.16 % 0.16"
      (list
        (rpn.code/declare-symbol-code "aA")
        (rpn.code/declare-symbol-code "jC")
        (rpn.code/push-symbol-code "jC")
        (rpn.code/push-symbol-code "aA")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.16)
        (rpn.code/push-code 1.36)
        (rpn.code/push-code 1.78)
        (rpn.code/max-code 2)
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.6)
        (rpn.code/push-code 0.13)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.77)
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.09)
        rpn.code/power-code
        (rpn.code/push-code 1.16)
        rpn.code/power-code
        (rpn.code/push-code 0.16)
        rpn.code/modulo-code)]
    ["( kI ^ ( hb min lC ) )"
      (list
        (rpn.code/declare-symbol-code "hb")
        (rpn.code/declare-symbol-code "kI")
        (rpn.code/declare-symbol-code "lC")
        (rpn.code/push-symbol-code "kI")
        (rpn.code/push-symbol-code "hb")
        (rpn.code/push-symbol-code "lC")
        (rpn.code/min-code 2)
        rpn.code/power-code)]
    ["QQ + ( 0.36 )"
      (list
        (rpn.code/declare-symbol-code "QQ")
        (rpn.code/push-symbol-code "QQ")
        (rpn.code/push-code 0.36)
        (rpn.code/add-code 2))]
    ["lH + rJ min 1.48 - 0.60"
      (list
        (rpn.code/declare-symbol-code "lH")
        (rpn.code/declare-symbol-code "rJ")
        (rpn.code/push-symbol-code "lH")
        (rpn.code/push-symbol-code "rJ")
        (rpn.code/push-code 1.48)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.6)
        (rpn.code/subtract-code 2))]
    ["0.95 * ( 0.87 % 0.62 - ( 1.86 * 1.60 / ( ( NE min 1.72 / 0.13 * ( ( ( RJ / 1.91 ) * 1.23 max 0.91 ) ) ) max ( ( 1.33 * ( XQ min ( ( TM ) ) ) / ( ( 0.81 ) % DL ) ) ) / 0.97 ^ 0.94 ) ) max hh ) min kq"
      (list
        (rpn.code/declare-symbol-code "DL")
        (rpn.code/declare-symbol-code "NE")
        (rpn.code/declare-symbol-code "RJ")
        (rpn.code/declare-symbol-code "TM")
        (rpn.code/declare-symbol-code "XQ")
        (rpn.code/declare-symbol-code "hh")
        (rpn.code/declare-symbol-code "kq")
        (rpn.code/push-code 0.95)
        (rpn.code/push-code 0.87)
        (rpn.code/push-code 0.62)
        rpn.code/modulo-code
        (rpn.code/push-code 1.86)
        (rpn.code/push-code 1.6)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "NE")
        (rpn.code/push-code 1.72)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.13)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "RJ")
        (rpn.code/push-code 1.91)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.23)
        (rpn.code/push-code 0.91)
        (rpn.code/max-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.33)
        (rpn.code/push-symbol-code "XQ")
        (rpn.code/push-symbol-code "TM")
        (rpn.code/min-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.81)
        (rpn.code/push-symbol-code "DL")
        rpn.code/modulo-code
        (rpn.code/divide-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.97)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.94)
        rpn.code/power-code
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "hh")
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "kq")
        (rpn.code/min-code 2)
        (rpn.code/multiply-code 2))]
    ["( 0.07 min Oo + 1.97 ^ 0.89 ) max 0.37 + hO"
      (list
        (rpn.code/declare-symbol-code "Oo")
        (rpn.code/declare-symbol-code "hO")
        (rpn.code/push-code 0.07)
        (rpn.code/push-symbol-code "Oo")
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.97)
        (rpn.code/push-code 0.89)
        rpn.code/power-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.37)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "hO")
        (rpn.code/add-code 2))]
    ["( 1.22 - ( Rv - vU ) ) ^ XS / ( EL ) max ( ( 1.77 max ( ( uI * HP ^ ql ) max ( 1.31 max AW ) ) max 0.24 ^ 0.12 ) )"
      (list
        (rpn.code/declare-symbol-code "AW")
        (rpn.code/declare-symbol-code "EL")
        (rpn.code/declare-symbol-code "HP")
        (rpn.code/declare-symbol-code "Rv")
        (rpn.code/declare-symbol-code "XS")
        (rpn.code/declare-symbol-code "ql")
        (rpn.code/declare-symbol-code "uI")
        (rpn.code/declare-symbol-code "vU")
        (rpn.code/push-code 1.22)
        (rpn.code/push-symbol-code "Rv")
        (rpn.code/push-symbol-code "vU")
        (rpn.code/subtract-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "XS")
        rpn.code/power-code
        (rpn.code/push-symbol-code "EL")
        (rpn.code/push-code 1.77)
        (rpn.code/push-symbol-code "uI")
        (rpn.code/push-symbol-code "HP")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "ql")
        rpn.code/power-code
        (rpn.code/push-code 1.31)
        (rpn.code/push-symbol-code "AW")
        (rpn.code/max-code 2)
        (rpn.code/max-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.24)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.12)
        rpn.code/power-code
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2))]
    ["kp % ( 1.41 + ( 0.96 * ( ( ( ( MI * ( ( ( 0.32 max ( 0.42 ) ) ) + ( ( 0.01 min 1.73 - ( 1.87 ) min Ba ) * ( 1.06 max 0.83 + 1.90 ) ) + Wi * Qf ) ) ) % 0.85 min 0.05 ) ) % 1.64 ) + 0.60 ) ^ ng ^ hk"
      (list
        (rpn.code/declare-symbol-code "Ba")
        (rpn.code/declare-symbol-code "MI")
        (rpn.code/declare-symbol-code "Qf")
        (rpn.code/declare-symbol-code "Wi")
        (rpn.code/declare-symbol-code "hk")
        (rpn.code/declare-symbol-code "kp")
        (rpn.code/declare-symbol-code "ng")
        (rpn.code/push-symbol-code "kp")
        (rpn.code/push-code 1.41)
        (rpn.code/push-code 0.96)
        (rpn.code/push-symbol-code "MI")
        (rpn.code/push-code 0.32)
        (rpn.code/push-code 0.42)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.01)
        (rpn.code/push-code 1.73)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.87)
        (rpn.code/push-symbol-code "Ba")
        (rpn.code/min-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.06)
        (rpn.code/push-code 0.83)
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.9)
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "Wi")
        (rpn.code/push-symbol-code "Qf")
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.85)
        (rpn.code/push-code 0.05)
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.64)
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.6)
        (rpn.code/add-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "ng")
        rpn.code/power-code
        (rpn.code/push-symbol-code "hk")
        rpn.code/power-code)]
    ["Vz - ( Ol max 1.86 ) / 1.45"
      (list
        (rpn.code/declare-symbol-code "Ol")
        (rpn.code/declare-symbol-code "Vz")
        (rpn.code/push-symbol-code "Vz")
        (rpn.code/push-symbol-code "Ol")
        (rpn.code/push-code 1.86)
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.45)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2))]
    ["Uq"
      (list
        (rpn.code/declare-symbol-code "Uq")
        (rpn.code/push-symbol-code "Uq"))]
    ["( ts - ( 1.47 + ( pl ^ 1.65 min 0.06 min 1.28 ) ) * 0.08 + nf ) + SS"
      (list
        (rpn.code/declare-symbol-code "SS")
        (rpn.code/declare-symbol-code "nf")
        (rpn.code/declare-symbol-code "pl")
        (rpn.code/declare-symbol-code "ts")
        (rpn.code/push-symbol-code "ts")
        (rpn.code/push-code 1.47)
        (rpn.code/push-symbol-code "pl")
        (rpn.code/push-code 1.65)
        (rpn.code/push-code 0.06)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.28)
        (rpn.code/min-code 2)
        rpn.code/power-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.08)
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "nf")
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "SS")
        (rpn.code/add-code 2))]
    ["( 1.91 ) * ra + ( 1.79 max 0.52 min 0.32 + 1.54 ) - eM"
      (list
        (rpn.code/declare-symbol-code "eM")
        (rpn.code/declare-symbol-code "ra")
        (rpn.code/push-code 1.91)
        (rpn.code/push-symbol-code "ra")
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.79)
        (rpn.code/push-code 0.52)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.32)
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.54)
        (rpn.code/add-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "eM")
        (rpn.code/subtract-code 2))]
    ["nq % ( Oh / xW ^ GI )"
      (list
        (rpn.code/declare-symbol-code "GI")
        (rpn.code/declare-symbol-code "Oh")
        (rpn.code/declare-symbol-code "nq")
        (rpn.code/declare-symbol-code "xW")
        (rpn.code/push-symbol-code "nq")
        (rpn.code/push-symbol-code "Oh")
        (rpn.code/push-symbol-code "xW")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "GI")
        rpn.code/power-code
        rpn.code/modulo-code)]
    ["0.78 + Of"
      (list
        (rpn.code/declare-symbol-code "Of")
        (rpn.code/push-code 0.78)
        (rpn.code/push-symbol-code "Of")
        (rpn.code/add-code 2))]
    ["( 1.27 % ( Tr ^ 1.82 ) * TS )"
      (list
        (rpn.code/declare-symbol-code "TS")
        (rpn.code/declare-symbol-code "Tr")
        (rpn.code/push-code 1.27)
        (rpn.code/push-symbol-code "Tr")
        (rpn.code/push-code 1.82)
        rpn.code/power-code
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "TS")
        (rpn.code/multiply-code 2))]
    ["( tY - 1.39 * iw )"
      (list
        (rpn.code/declare-symbol-code "iw")
        (rpn.code/declare-symbol-code "tY")
        (rpn.code/push-symbol-code "tY")
        (rpn.code/push-code 1.39)
        (rpn.code/push-symbol-code "iw")
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2))]
    ["( UF ) / ( ( ( aK ^ 0.46 / 1.92 + ( ( Rw max Xo ) + 1.26 * ( 1.48 * ML + 0.47 - 0.03 ) ) ) - vG + 0.97 * XG ) * ( 0.71 + ( 1.45 - ( ( ( ( ( 1.07 max ( xs max ( ( ( wr max PP / 0.66 % 0.08 ) / 0.14 max ( qF ^ ( ( 0.12 % ( ( ( 1.82 - Dv ) ) ^ ( 1.93 ) % Xk min cn ) ) % ( Md % ( 1.05 max ( 1.39 max ( ( 1.21 ) / iE + Ca ) ) min ( fz ^ ( 1.36 max st - 1.50 * ( 1.81 ^ Pk ) ) ) ) + 0.67 min 0.59 ) / ( KW ) ) % ( Hv ) / 0.77 ) * Jq ) - 1.70 max oM ) / 0.37 + ( lT - ER ^ ( ( 1.83 ) ^ zv ) max ( 1.54 / ( 1.30 + 1.60 ) % uo ) ) ) min ( 0.96 ^ 0.18 % iK + ( xj ^ ( RE ) max 0.78 ) ) ) / JJ + WA % ( 1.78 ) ) ^ 0.61 + ( 1.69 % ( zO - ( Uj max ( ( mO ) ) ) min ( Ig + IH % yH ) / ( ( ( ( 0.31 - ( Vi + vy / ( 0.45 % 0.27 / 1.24 * ( 1.85 max zR / vg ) ) ) ) * YI % Dw ^ wb ) - 1.57 min 1.24 ) ) ) % ( 1.33 ) + QC ) + ( 0.16 - Wl ) ) ) + 1.56 + ( 1.84 ^ gl ^ ( 0.20 ) % ( UB + ( qX / ( 1.75 ^ 0.42 * ( ( 0.79 max 0.49 ) ^ zV % nV max ( ( 0.40 max Yn ) max ( 0.55 - Bj - ( Kl ) + 1.37 ) max Ps % 0.71 ) ) % 0.13 ) ) ) ) ) ) ^ 0.99 - ( ( wg min 0.95 - aS + ( Lb max FO * ( vZ * 1.25 ) ) ) min Ep ^ ( xZ / Av ^ DQ ^ Tk ) max gJ ) ) max GJ max 0.41 ) ^ 0.26 max ( ra * ( xY % bI max 1.21 / 1.97 ) - EJ )"
      (list
        (rpn.code/declare-symbol-code "Av")
        (rpn.code/declare-symbol-code "Bj")
        (rpn.code/declare-symbol-code "Ca")
        (rpn.code/declare-symbol-code "DQ")
        (rpn.code/declare-symbol-code "Dv")
        (rpn.code/declare-symbol-code "Dw")
        (rpn.code/declare-symbol-code "EJ")
        (rpn.code/declare-symbol-code "ER")
        (rpn.code/declare-symbol-code "Ep")
        (rpn.code/declare-symbol-code "FO")
        (rpn.code/declare-symbol-code "GJ")
        (rpn.code/declare-symbol-code "Hv")
        (rpn.code/declare-symbol-code "IH")
        (rpn.code/declare-symbol-code "Ig")
        (rpn.code/declare-symbol-code "JJ")
        (rpn.code/declare-symbol-code "Jq")
        (rpn.code/declare-symbol-code "KW")
        (rpn.code/declare-symbol-code "Kl")
        (rpn.code/declare-symbol-code "Lb")
        (rpn.code/declare-symbol-code "ML")
        (rpn.code/declare-symbol-code "Md")
        (rpn.code/declare-symbol-code "PP")
        (rpn.code/declare-symbol-code "Pk")
        (rpn.code/declare-symbol-code "Ps")
        (rpn.code/declare-symbol-code "QC")
        (rpn.code/declare-symbol-code "RE")
        (rpn.code/declare-symbol-code "Rw")
        (rpn.code/declare-symbol-code "Tk")
        (rpn.code/declare-symbol-code "UB")
        (rpn.code/declare-symbol-code "UF")
        (rpn.code/declare-symbol-code "Uj")
        (rpn.code/declare-symbol-code "Vi")
        (rpn.code/declare-symbol-code "WA")
        (rpn.code/declare-symbol-code "Wl")
        (rpn.code/declare-symbol-code "XG")
        (rpn.code/declare-symbol-code "Xk")
        (rpn.code/declare-symbol-code "Xo")
        (rpn.code/declare-symbol-code "YI")
        (rpn.code/declare-symbol-code "Yn")
        (rpn.code/declare-symbol-code "aK")
        (rpn.code/declare-symbol-code "aS")
        (rpn.code/declare-symbol-code "bI")
        (rpn.code/declare-symbol-code "cn")
        (rpn.code/declare-symbol-code "fz")
        (rpn.code/declare-symbol-code "gJ")
        (rpn.code/declare-symbol-code "gl")
        (rpn.code/declare-symbol-code "iE")
        (rpn.code/declare-symbol-code "iK")
        (rpn.code/declare-symbol-code "lT")
        (rpn.code/declare-symbol-code "mO")
        (rpn.code/declare-symbol-code "nV")
        (rpn.code/declare-symbol-code "oM")
        (rpn.code/declare-symbol-code "qF")
        (rpn.code/declare-symbol-code "qX")
        (rpn.code/declare-symbol-code "ra")
        (rpn.code/declare-symbol-code "st")
        (rpn.code/declare-symbol-code "uo")
        (rpn.code/declare-symbol-code "vG")
        (rpn.code/declare-symbol-code "vZ")
        (rpn.code/declare-symbol-code "vg")
        (rpn.code/declare-symbol-code "vy")
        (rpn.code/declare-symbol-code "wb")
        (rpn.code/declare-symbol-code "wg")
        (rpn.code/declare-symbol-code "wr")
        (rpn.code/declare-symbol-code "xY")
        (rpn.code/declare-symbol-code "xZ")
        (rpn.code/declare-symbol-code "xj")
        (rpn.code/declare-symbol-code "xs")
        (rpn.code/declare-symbol-code "yH")
        (rpn.code/declare-symbol-code "zO")
        (rpn.code/declare-symbol-code "zR")
        (rpn.code/declare-symbol-code "zV")
        (rpn.code/declare-symbol-code "zv")
        (rpn.code/push-symbol-code "UF")
        (rpn.code/push-symbol-code "aK")
        (rpn.code/push-code 0.46)
        rpn.code/power-code
        (rpn.code/push-code 1.92)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Rw")
        (rpn.code/push-symbol-code "Xo")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.26)
        (rpn.code/push-code 1.48)
        (rpn.code/push-symbol-code "ML")
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.47)
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.03)
        (rpn.code/subtract-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "vG")
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.97)
        (rpn.code/push-symbol-code "XG")
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.71)
        (rpn.code/push-code 1.45)
        (rpn.code/push-code 1.07)
        (rpn.code/push-symbol-code "xs")
        (rpn.code/push-symbol-code "wr")
        (rpn.code/push-symbol-code "PP")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.66)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.08)
        rpn.code/modulo-code
        (rpn.code/push-code 0.14)
        (rpn.code/push-symbol-code "qF")
        (rpn.code/push-code 0.12)
        (rpn.code/push-code 1.82)
        (rpn.code/push-symbol-code "Dv")
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.93)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Xk")
        (rpn.code/push-symbol-code "cn")
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "Md")
        (rpn.code/push-code 1.05)
        (rpn.code/push-code 1.39)
        (rpn.code/push-code 1.21)
        (rpn.code/push-symbol-code "iE")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Ca")
        (rpn.code/add-code 2)
        (rpn.code/max-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "fz")
        (rpn.code/push-code 1.36)
        (rpn.code/push-symbol-code "st")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.5)
        (rpn.code/push-code 1.81)
        (rpn.code/push-symbol-code "Pk")
        rpn.code/power-code
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2)
        rpn.code/power-code
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 0.67)
        (rpn.code/push-code 0.59)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "KW")
        (rpn.code/divide-code 2)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Hv")
        rpn.code/modulo-code
        (rpn.code/push-code 0.77)
        (rpn.code/divide-code 2)
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Jq")
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.7)
        (rpn.code/push-symbol-code "oM")
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.37)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "lT")
        (rpn.code/push-symbol-code "ER")
        (rpn.code/push-code 1.83)
        (rpn.code/push-symbol-code "zv")
        rpn.code/power-code
        (rpn.code/push-code 1.54)
        (rpn.code/push-code 1.3)
        (rpn.code/push-code 1.6)
        (rpn.code/add-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "uo")
        rpn.code/modulo-code
        (rpn.code/max-code 2)
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/add-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.96)
        (rpn.code/push-code 0.18)
        rpn.code/power-code
        (rpn.code/push-symbol-code "iK")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "xj")
        (rpn.code/push-symbol-code "RE")
        (rpn.code/push-code 0.78)
        (rpn.code/max-code 2)
        rpn.code/power-code
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "JJ")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "WA")
        (rpn.code/push-code 1.78)
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.61)
        rpn.code/power-code
        (rpn.code/push-code 1.69)
        (rpn.code/push-symbol-code "zO")
        (rpn.code/push-symbol-code "Uj")
        (rpn.code/push-symbol-code "mO")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "Ig")
        (rpn.code/push-symbol-code "IH")
        (rpn.code/push-symbol-code "yH")
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.31)
        (rpn.code/push-symbol-code "Vi")
        (rpn.code/push-symbol-code "vy")
        (rpn.code/push-code 0.45)
        (rpn.code/push-code 0.27)
        rpn.code/modulo-code
        (rpn.code/push-code 1.24)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.85)
        (rpn.code/push-symbol-code "zR")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "vg")
        (rpn.code/divide-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "YI")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "Dw")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "wb")
        rpn.code/power-code
        (rpn.code/push-code 1.57)
        (rpn.code/push-code 1.24)
        (rpn.code/min-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/subtract-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 1.33)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "QC")
        (rpn.code/add-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.16)
        (rpn.code/push-symbol-code "Wl")
        (rpn.code/subtract-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.56)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.84)
        (rpn.code/push-symbol-code "gl")
        rpn.code/power-code
        (rpn.code/push-code 0.2)
        rpn.code/power-code
        (rpn.code/push-symbol-code "UB")
        (rpn.code/push-symbol-code "qX")
        (rpn.code/push-code 1.75)
        (rpn.code/push-code 0.42)
        rpn.code/power-code
        (rpn.code/push-code 0.79)
        (rpn.code/push-code 0.49)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "zV")
        rpn.code/power-code
        (rpn.code/push-symbol-code "nV")
        (rpn.code/push-code 0.4)
        (rpn.code/push-symbol-code "Yn")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.55)
        (rpn.code/push-symbol-code "Bj")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Kl")
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.37)
        (rpn.code/add-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "Ps")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.71)
        rpn.code/modulo-code
        (rpn.code/max-code 2)
        rpn.code/modulo-code
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.13)
        rpn.code/modulo-code
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.99)
        rpn.code/power-code
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "wg")
        (rpn.code/push-code 0.95)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "aS")
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "Lb")
        (rpn.code/push-symbol-code "FO")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "vZ")
        (rpn.code/push-code 1.25)
        (rpn.code/multiply-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "Ep")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "xZ")
        (rpn.code/push-symbol-code "Av")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "DQ")
        rpn.code/power-code
        (rpn.code/push-symbol-code "Tk")
        rpn.code/power-code
        (rpn.code/push-symbol-code "gJ")
        (rpn.code/max-code 2)
        rpn.code/power-code
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "GJ")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.41)
        (rpn.code/max-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.26)
        (rpn.code/push-symbol-code "ra")
        (rpn.code/push-symbol-code "xY")
        (rpn.code/push-symbol-code "bI")
        (rpn.code/push-code 1.21)
        (rpn.code/max-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 1.97)
        (rpn.code/divide-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "EJ")
        (rpn.code/subtract-code 2)
        (rpn.code/max-code 2)
        rpn.code/power-code)]
    ["( 0.57 + ( zl ) ) / ( uE ) + ( QX max ( 0.88 ) ^ KV ) min ( ( 1.78 % 1.65 ) + 1.93 )"
      (list
        (rpn.code/declare-symbol-code "KV")
        (rpn.code/declare-symbol-code "QX")
        (rpn.code/declare-symbol-code "uE")
        (rpn.code/declare-symbol-code "zl")
        (rpn.code/push-code 0.57)
        (rpn.code/push-symbol-code "zl")
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "uE")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "QX")
        (rpn.code/push-code 0.88)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "KV")
        rpn.code/power-code
        (rpn.code/push-code 1.78)
        (rpn.code/push-code 1.65)
        rpn.code/modulo-code
        (rpn.code/push-code 1.93)
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2))]
    ["( 0.47 ) max 0.99 + 1.52"
      (list
        (rpn.code/push-code 0.47)
        (rpn.code/push-code 0.99)
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.52)
        (rpn.code/add-code 2))]
    ["vo / ( pG )"
      (list
        (rpn.code/declare-symbol-code "pG")
        (rpn.code/declare-symbol-code "vo")
        (rpn.code/push-symbol-code "vo")
        (rpn.code/push-symbol-code "pG")
        (rpn.code/divide-code 2))]
    ["0.62 % oA"
      (list
        (rpn.code/declare-symbol-code "oA")
        (rpn.code/push-code 0.62)
        (rpn.code/push-symbol-code "oA")
        rpn.code/modulo-code)]
    ["( 1.40 )"
      (list
        (rpn.code/push-code 1.4))]
    ["ko max kt * dq"
      (list
        (rpn.code/declare-symbol-code "dq")
        (rpn.code/declare-symbol-code "ko")
        (rpn.code/declare-symbol-code "kt")
        (rpn.code/push-symbol-code "ko")
        (rpn.code/push-symbol-code "kt")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "dq")
        (rpn.code/multiply-code 2))]
    ["( ( bY ) * ( sm * sd ) * 1.76 / 0.22 )"
      (list
        (rpn.code/declare-symbol-code "bY")
        (rpn.code/declare-symbol-code "sd")
        (rpn.code/declare-symbol-code "sm")
        (rpn.code/push-symbol-code "bY")
        (rpn.code/push-symbol-code "sm")
        (rpn.code/push-symbol-code "sd")
        (rpn.code/multiply-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 1.76)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.22)
        (rpn.code/divide-code 2))]
    ["1.60 min 1.25 + PW"
      (list
        (rpn.code/declare-symbol-code "PW")
        (rpn.code/push-code 1.6)
        (rpn.code/push-code 1.25)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "PW")
        (rpn.code/add-code 2))]
    ["( ( 0.25 ) ^ 1.51 )"
      (list
        (rpn.code/push-code 0.25)
        (rpn.code/push-code 1.51)
        rpn.code/power-code)]
    ["uT * rL * WE"
      (list
        (rpn.code/declare-symbol-code "WE")
        (rpn.code/declare-symbol-code "rL")
        (rpn.code/declare-symbol-code "uT")
        (rpn.code/push-symbol-code "uT")
        (rpn.code/push-symbol-code "rL")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "WE")
        (rpn.code/multiply-code 2))]
    ["1.39 max WJ * 1.41"
      (list
        (rpn.code/declare-symbol-code "WJ")
        (rpn.code/push-code 1.39)
        (rpn.code/push-symbol-code "WJ")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.41)
        (rpn.code/multiply-code 2))]
    ["VV min NQ / nG / ( nf ^ FU - ts )"
      (list
        (rpn.code/declare-symbol-code "FU")
        (rpn.code/declare-symbol-code "NQ")
        (rpn.code/declare-symbol-code "VV")
        (rpn.code/declare-symbol-code "nG")
        (rpn.code/declare-symbol-code "nf")
        (rpn.code/declare-symbol-code "ts")
        (rpn.code/push-symbol-code "VV")
        (rpn.code/push-symbol-code "NQ")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "nG")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "nf")
        (rpn.code/push-symbol-code "FU")
        rpn.code/power-code
        (rpn.code/push-symbol-code "ts")
        (rpn.code/subtract-code 2)
        (rpn.code/divide-code 2))]
    ["1.65 min ( ( dx / ( 1.12 ^ ( ( Np + vf % 0.95 - ( 0.23 / 1.08 + ( Oh ^ 1.58 / ( 1.60 min mw % ( ( xp % ( LU ) / 1.15 ) min Uw max 1.23 + 1.04 ) ^ Sa ) ) + NV ) ) - 0.03 ) ^ Eo max UQ ) ) ^ zR - ( 0.14 ) * 1.93 )"
      (list
        (rpn.code/declare-symbol-code "Eo")
        (rpn.code/declare-symbol-code "LU")
        (rpn.code/declare-symbol-code "NV")
        (rpn.code/declare-symbol-code "Np")
        (rpn.code/declare-symbol-code "Oh")
        (rpn.code/declare-symbol-code "Sa")
        (rpn.code/declare-symbol-code "UQ")
        (rpn.code/declare-symbol-code "Uw")
        (rpn.code/declare-symbol-code "dx")
        (rpn.code/declare-symbol-code "mw")
        (rpn.code/declare-symbol-code "vf")
        (rpn.code/declare-symbol-code "xp")
        (rpn.code/declare-symbol-code "zR")
        (rpn.code/push-code 1.65)
        (rpn.code/push-symbol-code "dx")
        (rpn.code/push-code 1.12)
        (rpn.code/push-symbol-code "Np")
        (rpn.code/push-symbol-code "vf")
        (rpn.code/push-code 0.95)
        rpn.code/modulo-code
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.23)
        (rpn.code/push-code 1.08)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Oh")
        (rpn.code/push-code 1.58)
        rpn.code/power-code
        (rpn.code/push-code 1.6)
        (rpn.code/push-symbol-code "mw")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "xp")
        (rpn.code/push-symbol-code "LU")
        rpn.code/modulo-code
        (rpn.code/push-code 1.15)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Uw")
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.23)
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.04)
        (rpn.code/add-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "Sa")
        rpn.code/power-code
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "NV")
        (rpn.code/add-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.03)
        (rpn.code/subtract-code 2)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Eo")
        (rpn.code/push-symbol-code "UQ")
        (rpn.code/max-code 2)
        rpn.code/power-code
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "zR")
        rpn.code/power-code
        (rpn.code/push-code 0.14)
        (rpn.code/push-code 1.93)
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/min-code 2))]
    ["Pc + 0.94"
      (list
        (rpn.code/declare-symbol-code "Pc")
        (rpn.code/push-symbol-code "Pc")
        (rpn.code/push-code 0.94)
        (rpn.code/add-code 2))]
    ["Pj ^ ( IB + Ng ) min 0.30"
      (list
        (rpn.code/declare-symbol-code "IB")
        (rpn.code/declare-symbol-code "Ng")
        (rpn.code/declare-symbol-code "Pj")
        (rpn.code/push-symbol-code "Pj")
        (rpn.code/push-symbol-code "IB")
        (rpn.code/push-symbol-code "Ng")
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.3)
        (rpn.code/min-code 2)
        rpn.code/power-code)]
    ["( 1.87 ) max KR % 0.50"
      (list
        (rpn.code/declare-symbol-code "KR")
        (rpn.code/push-code 1.87)
        (rpn.code/push-symbol-code "KR")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.5)
        rpn.code/modulo-code)]
    ["( ( ( 0.18 / kq % gq / 1.50 ) ) % ( Ps ) - ( 1.53 ) max ( ( 1.89 min ( ( 1.24 - ( 0.28 ) ) - vO * HU ) / ( cr max 0.79 min ( 0.31 ) ) * dC ) max zG max Hs min ( 1.81 + 1.55 / 0.68 + 0.16 ) ) ) / ( ( 0.24 ) max ( Lf max ( ( 0.59 - ( ( Jo + ck min 0.77 min ( 0.18 ) ) / jG % ( 1.61 ) ) ) ) ^ Yd min ( dZ ^ ZJ + 1.67 ) ) / 0.85 + ( nv ) )"
      (list
        (rpn.code/declare-symbol-code "HU")
        (rpn.code/declare-symbol-code "Hs")
        (rpn.code/declare-symbol-code "Jo")
        (rpn.code/declare-symbol-code "Lf")
        (rpn.code/declare-symbol-code "Ps")
        (rpn.code/declare-symbol-code "Yd")
        (rpn.code/declare-symbol-code "ZJ")
        (rpn.code/declare-symbol-code "ck")
        (rpn.code/declare-symbol-code "cr")
        (rpn.code/declare-symbol-code "dC")
        (rpn.code/declare-symbol-code "dZ")
        (rpn.code/declare-symbol-code "gq")
        (rpn.code/declare-symbol-code "jG")
        (rpn.code/declare-symbol-code "kq")
        (rpn.code/declare-symbol-code "nv")
        (rpn.code/declare-symbol-code "vO")
        (rpn.code/declare-symbol-code "zG")
        (rpn.code/push-code 0.18)
        (rpn.code/push-symbol-code "kq")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "gq")
        rpn.code/modulo-code
        (rpn.code/push-code 1.5)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "Ps")
        rpn.code/modulo-code
        (rpn.code/push-code 1.53)
        (rpn.code/push-code 1.89)
        (rpn.code/push-code 1.24)
        (rpn.code/push-code 0.28)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "vO")
        (rpn.code/push-symbol-code "HU")
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "cr")
        (rpn.code/push-code 0.79)
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.31)
        (rpn.code/min-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "dC")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "zG")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "Hs")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.81)
        (rpn.code/push-code 1.55)
        (rpn.code/push-code 0.68)
        (rpn.code/divide-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-code 0.16)
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 0.24)
        (rpn.code/push-symbol-code "Lf")
        (rpn.code/push-code 0.59)
        (rpn.code/push-symbol-code "Jo")
        (rpn.code/push-symbol-code "ck")
        (rpn.code/push-code 0.77)
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.18)
        (rpn.code/min-code 2)
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "jG")
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.61)
        rpn.code/modulo-code
        (rpn.code/subtract-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "Yd")
        (rpn.code/push-symbol-code "dZ")
        (rpn.code/push-symbol-code "ZJ")
        rpn.code/power-code
        (rpn.code/push-code 1.67)
        (rpn.code/add-code 2)
        (rpn.code/min-code 2)
        rpn.code/power-code
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.85)
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "nv")
        (rpn.code/add-code 2)
        (rpn.code/divide-code 2))]
    ["( fA ) ^ 0.61"
      (list
        (rpn.code/declare-symbol-code "fA")
        (rpn.code/push-symbol-code "fA")
        (rpn.code/push-code 0.61)
        rpn.code/power-code)]
    ["1.06"
      (list
        (rpn.code/push-code 1.06))]
    ["Jl % cx - 1.37 * UI"
      (list
        (rpn.code/declare-symbol-code "Jl")
        (rpn.code/declare-symbol-code "UI")
        (rpn.code/declare-symbol-code "cx")
        (rpn.code/push-symbol-code "Jl")
        (rpn.code/push-symbol-code "cx")
        rpn.code/modulo-code
        (rpn.code/push-code 1.37)
        (rpn.code/push-symbol-code "UI")
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2))]
    ["( ( 1.90 ) ) - SM * ( ( ( ( 1.69 - ( ( ( Xs ) / ( 1.45 ) ^ 0.59 ) max qD ) ) % ( 1.11 * Wy ) * 1.47 min 1.42 ) min Gk % Af * ( ( IJ * 0.65 ) * 0.34 ) ) )"
      (list
        (rpn.code/declare-symbol-code "Af")
        (rpn.code/declare-symbol-code "Gk")
        (rpn.code/declare-symbol-code "IJ")
        (rpn.code/declare-symbol-code "SM")
        (rpn.code/declare-symbol-code "Wy")
        (rpn.code/declare-symbol-code "Xs")
        (rpn.code/declare-symbol-code "qD")
        (rpn.code/push-code 1.9)
        (rpn.code/push-symbol-code "SM")
        (rpn.code/push-code 1.69)
        (rpn.code/push-symbol-code "Xs")
        (rpn.code/push-code 1.45)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.59)
        rpn.code/power-code
        (rpn.code/push-symbol-code "qD")
        (rpn.code/max-code 2)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.11)
        (rpn.code/push-symbol-code "Wy")
        (rpn.code/multiply-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 1.47)
        (rpn.code/push-code 1.42)
        (rpn.code/min-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "Gk")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "Af")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "IJ")
        (rpn.code/push-code 0.65)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.34)
        (rpn.code/multiply-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/subtract-code 2))]
    ["1.56 min SS max 1.48"
      (list
        (rpn.code/declare-symbol-code "SS")
        (rpn.code/push-code 1.56)
        (rpn.code/push-symbol-code "SS")
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.48)
        (rpn.code/max-code 2))]
    ["1.15 ^ 1.96 ^ ( Me max yi min 0.71 )"
      (list
        (rpn.code/declare-symbol-code "Me")
        (rpn.code/declare-symbol-code "yi")
        (rpn.code/push-code 1.15)
        (rpn.code/push-code 1.96)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Me")
        (rpn.code/push-symbol-code "yi")
        (rpn.code/max-code 2)
        (rpn.code/push-code 0.71)
        (rpn.code/min-code 2)
        rpn.code/power-code)]
    ["0.20"
      (list
        (rpn.code/push-code 0.2))]
    ["( 0.28 + 1.62 ) ^ 1.65 max 0.49"
      (list
        (rpn.code/push-code 0.28)
        (rpn.code/push-code 1.62)
        (rpn.code/add-code 2)
        (rpn.code/push-code 1.65)
        (rpn.code/push-code 0.49)
        (rpn.code/max-code 2)
        rpn.code/power-code)]
    ["( 1.34 - ( 1.61 ) )"
      (list
        (rpn.code/push-code 1.34)
        (rpn.code/push-code 1.61)
        (rpn.code/subtract-code 2))]
    ["0.60 max 0.33"
      (list
        (rpn.code/push-code 0.6)
        (rpn.code/push-code 0.33)
        (rpn.code/max-code 2))]
    ["xX max 0.68"
      (list
        (rpn.code/declare-symbol-code "xX")
        (rpn.code/push-symbol-code "xX")
        (rpn.code/push-code 0.68)
        (rpn.code/max-code 2))]
    ["Vt * ( 0.85 min Mt ) + 0.05 ^ ( 0.79 / ( 1.09 ^ 1.41 ) max Ef max HU )"
      (list
        (rpn.code/declare-symbol-code "Ef")
        (rpn.code/declare-symbol-code "HU")
        (rpn.code/declare-symbol-code "Mt")
        (rpn.code/declare-symbol-code "Vt")
        (rpn.code/push-symbol-code "Vt")
        (rpn.code/push-code 0.85)
        (rpn.code/push-symbol-code "Mt")
        (rpn.code/min-code 2)
        (rpn.code/multiply-code 2)
        (rpn.code/push-code 0.05)
        (rpn.code/push-code 0.79)
        (rpn.code/push-code 1.09)
        (rpn.code/push-code 1.41)
        rpn.code/power-code
        (rpn.code/push-symbol-code "Ef")
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "HU")
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2)
        rpn.code/power-code
        (rpn.code/add-code 2))]
    ["Tc max 0.15"
      (list
        (rpn.code/declare-symbol-code "Tc")
        (rpn.code/push-symbol-code "Tc")
        (rpn.code/push-code 0.15)
        (rpn.code/max-code 2))]
    ["1.79 min Af"
      (list
        (rpn.code/declare-symbol-code "Af")
        (rpn.code/push-code 1.79)
        (rpn.code/push-symbol-code "Af")
        (rpn.code/min-code 2))]
    ["1.29 * HQ"
      (list
        (rpn.code/declare-symbol-code "HQ")
        (rpn.code/push-code 1.29)
        (rpn.code/push-symbol-code "HQ")
        (rpn.code/multiply-code 2))]
    ["yr"
      (list
        (rpn.code/declare-symbol-code "yr")
        (rpn.code/push-symbol-code "yr"))]
    ["lf * Ha ^ kO"
      (list
        (rpn.code/declare-symbol-code "Ha")
        (rpn.code/declare-symbol-code "kO")
        (rpn.code/declare-symbol-code "lf")
        (rpn.code/push-symbol-code "lf")
        (rpn.code/push-symbol-code "Ha")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "kO")
        rpn.code/power-code)]
    ["1.82 max ( Mv min FE ) max rF ^ 1.47"
      (list
        (rpn.code/declare-symbol-code "FE")
        (rpn.code/declare-symbol-code "Mv")
        (rpn.code/declare-symbol-code "rF")
        (rpn.code/push-code 1.82)
        (rpn.code/push-symbol-code "Mv")
        (rpn.code/push-symbol-code "FE")
        (rpn.code/min-code 2)
        (rpn.code/max-code 2)
        (rpn.code/push-symbol-code "rF")
        (rpn.code/max-code 2)
        (rpn.code/push-code 1.47)
        rpn.code/power-code)]
    ["( Se % ( 1.76 ) ) * 1.68 + Yj"
      (list
        (rpn.code/declare-symbol-code "Se")
        (rpn.code/declare-symbol-code "Yj")
        (rpn.code/push-symbol-code "Se")
        (rpn.code/push-code 1.76)
        rpn.code/modulo-code
        (rpn.code/push-code 1.68)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "Yj")
        (rpn.code/add-code 2))]
    ["NE % 1.78 min My % ot"
      (list
        (rpn.code/declare-symbol-code "My")
        (rpn.code/declare-symbol-code "NE")
        (rpn.code/declare-symbol-code "ot")
        (rpn.code/push-symbol-code "NE")
        (rpn.code/push-code 1.78)
        (rpn.code/push-symbol-code "My")
        (rpn.code/min-code 2)
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "ot")
        rpn.code/modulo-code)]
    ["xn min PT / ug max 1.57"
      (list
        (rpn.code/declare-symbol-code "PT")
        (rpn.code/declare-symbol-code "ug")
        (rpn.code/declare-symbol-code "xn")
        (rpn.code/push-symbol-code "xn")
        (rpn.code/push-symbol-code "PT")
        (rpn.code/min-code 2)
        (rpn.code/push-symbol-code "ug")
        (rpn.code/push-code 1.57)
        (rpn.code/max-code 2)
        (rpn.code/divide-code 2))]
    ["Tp"
      (list
        (rpn.code/declare-symbol-code "Tp")
        (rpn.code/push-symbol-code "Tp"))]
    ["0.53 % 1.09 * 0.23 % ( uv / ( kP ) ^ ( ( PV ) min Lf ) max 0.37 )"
      (list
        (rpn.code/declare-symbol-code "Lf")
        (rpn.code/declare-symbol-code "PV")
        (rpn.code/declare-symbol-code "kP")
        (rpn.code/declare-symbol-code "uv")
        (rpn.code/push-code 0.53)
        (rpn.code/push-code 1.09)
        rpn.code/modulo-code
        (rpn.code/push-code 0.23)
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "uv")
        (rpn.code/push-symbol-code "kP")
        (rpn.code/divide-code 2)
        (rpn.code/push-symbol-code "PV")
        (rpn.code/push-symbol-code "Lf")
        (rpn.code/min-code 2)
        (rpn.code/push-code 0.37)
        (rpn.code/max-code 2)
        rpn.code/power-code
        rpn.code/modulo-code)]
    ["1.46"
      (list
        (rpn.code/push-code 1.46))]
    ["( qO min zM / 1.73 min gR ) / ( 0.28 % ( ( 0.14 - 0.19 - 1.32 ) / vC * 0.15 ) + 1.49 )"
      (list
        (rpn.code/declare-symbol-code "gR")
        (rpn.code/declare-symbol-code "qO")
        (rpn.code/declare-symbol-code "vC")
        (rpn.code/declare-symbol-code "zM")
        (rpn.code/push-symbol-code "qO")
        (rpn.code/push-symbol-code "zM")
        (rpn.code/min-code 2)
        (rpn.code/push-code 1.73)
        (rpn.code/push-symbol-code "gR")
        (rpn.code/min-code 2)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.28)
        (rpn.code/push-code 0.14)
        (rpn.code/push-code 0.19)
        (rpn.code/subtract-code 2)
        (rpn.code/push-code 1.32)
        (rpn.code/subtract-code 2)
        (rpn.code/push-symbol-code "vC")
        (rpn.code/divide-code 2)
        (rpn.code/push-code 0.15)
        (rpn.code/multiply-code 2)
        rpn.code/modulo-code
        (rpn.code/push-code 1.49)
        (rpn.code/add-code 2)
        (rpn.code/divide-code 2))]
    ["( QP % xC ) * zB + be"
      (list
        (rpn.code/declare-symbol-code "QP")
        (rpn.code/declare-symbol-code "be")
        (rpn.code/declare-symbol-code "xC")
        (rpn.code/declare-symbol-code "zB")
        (rpn.code/push-symbol-code "QP")
        (rpn.code/push-symbol-code "xC")
        rpn.code/modulo-code
        (rpn.code/push-symbol-code "zB")
        (rpn.code/multiply-code 2)
        (rpn.code/push-symbol-code "be")
        (rpn.code/add-code 2))]
    ["0.24 + uC max ( 1.52 ^ ( fu ^ ( Wr + 1.77 ^ 0.60 + Jd ) ) )"
      (list
        (rpn.code/declare-symbol-code "Jd")
        (rpn.code/declare-symbol-code "Wr")
        (rpn.code/declare-symbol-code "fu")
        (rpn.code/declare-symbol-code "uC")
        (rpn.code/push-code 0.24)
        (rpn.code/push-symbol-code "uC")
        (rpn.code/push-code 1.52)
        (rpn.code/push-symbol-code "fu")
        (rpn.code/push-symbol-code "Wr")
        (rpn.code/push-code 1.77)
        (rpn.code/push-code 0.6)
        rpn.code/power-code
        (rpn.code/add-code 2)
        (rpn.code/push-symbol-code "Jd")
        (rpn.code/add-code 2)
        rpn.code/power-code
        rpn.code/power-code
        (rpn.code/max-code 2)
        (rpn.code/add-code 2))]
    ["( pd % 1.18 / 1.04 % ( 1.29 - xz ) )"
      (list
        (rpn.code/declare-symbol-code "pd")
        (rpn.code/declare-symbol-code "xz")
        (rpn.code/push-symbol-code "pd")
        (rpn.code/push-code 1.18)
        rpn.code/modulo-code
        (rpn.code/push-code 1.04)
        (rpn.code/divide-code 2)
        (rpn.code/push-code 1.29)
        (rpn.code/push-symbol-code "xz")
        (rpn.code/subtract-code 2)
        rpn.code/modulo-code)]
    ["sv"
      (list
        (rpn.code/declare-symbol-code "sv")
        (rpn.code/push-symbol-code "sv"))]))
