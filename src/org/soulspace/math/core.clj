;;;;
;;;;   Copyright (c) Ludger Solbach. All rights reserved.
;;;;
;;;;   The use and distribution terms for this software are covered by the
;;;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;;   which can be found in the file license.txt at the root of this distribution.
;;;;   By using this software in any fashion, you are agreeing to be bound by
;;;;   the terms of this license.
;;;;
;;;;   You must not remove this notice, or any other, from this software.
;;;;
(ns org.soulspace.math.core)

;;;
;;; Java Math implementations for Clojure
;;;
(set! *warn-on-reflection* true)

(def default-epsilon "Default tolerance (epsilon)." 0.00001)
(def default-dx "Default step size (delta x)." 0.0000001)

(def PI (Math/PI))
(def E  (Math/E))
(def LN2 (Math/log 2))
(def DOUBLE-PI (* 2 PI))
(def HALF-PI (/ PI 2))

(defn abs
  "Calculates the absolute of x (with java.lang.Math)."
  ^double [^double x]
  (Math/abs x))

(defn sign
  "Calculates the sign of x (with java.lang.Math)."
  ^double [^double x]
  (Math/signum x))

(defn floor
  "Calculates the floor of x (with java.lang.Math)."
  [^double x]
  (Math/floor x))

(defn ceil
  "Calculates the ceiling of x (with java.lang.Math)."
  [^double x]
  (Math/ceil x))

(defn sqrt
  "Calculates the square root of x (with java.lang.Math)."
  [^double x]
  (Math/sqrt x))

(defn cbrt
  "Calculates the cubic root of x (with java.lang.Math)."
  [^double x]
  (Math/cbrt x))

(defn pow
  "Calculates x raised to the power of y (with java.lang.Math)."
  [^double x ^double y]
  (Math/pow x y))

(defn exp
  "Calculates the exponential function of x (with java.lang.Math)."
  [^double x]
  (Math/exp x))

(defn expm1
  "Calulates e to the power of x minus 1."
  [^double x]
  (Math/expm1 x))

(defn log
  "Calculates the natural logarithm (with base e) of x (with java.lang.Math)."
  [^double x]
  (Math/log x))

(defn log-with-base
  "Calculates the logarithm with base b of x."
  [^double b ^double x]
  (/ (Math/log x) (Math/log b)))

(defn log10
  "Calculates the logarithm with base 10 of x (with java.lang.Math)."
  [^double x]
  (Math/log10 x))

(defn alog10
  "Calculates the inverse of the logarithm with base 2 of x (with java.lang.Math)."
  [^double x]
  (Math/pow 10 x))

(defn log2
  "Calculates the logarithm with base 2 of x (with java.lang.Math)."
  [^double x]
  (/ (Math/log x) LN2))

(defn alog2
  "Calculates the inverse of the logarithm with base 2 of x (with java.lang.Math)."
  [^double x]
  (Math/pow 2 x))

(defn log1p
  "Calculates the natural logarithm of the sum of x and 1."
  [^double x]
  (Math/log1p x))

; Trigonometrical functions

(defn cos
  [^double x]
  "Calculates the cosine of x (with java.lang.Math)."
  (Math/cos x))

(defn sin
  "Calculates the sine of x (with java.lang.Math)."
  [^double x]
  (Math/sin x))

(defn tan
  "Calculates the tangens of x (with java.lang.Math)."
  [^double x]
  (Math/tan x))

(defn asin
  "Calculates the arc sine of x (with java.lang.Math)."
  [^double x]
  (Math/asin x))

(defn acos
  "Calculates the arc cosine of x (with java.lang.Math)."
  [^double x]
  (Math/acos x))

(defn atan
  "Calculates the arc tangens of x (with java.lang.Math)."
  [^double x]
  (Math/atan x))

(defn atan2 [a b]
  "Calculates the arc tangens of a/b. Returns the result in the correct quadrant."
  (let [r (atan (/ a b))] ; TODO handle b = 0 case
    (cond
      (< b 0) (if (< a 0)
                (- r PI) ; adjust quadrant
                (+ r PI)) ; adjust quadrant
      :default r)))

(defn cosh
  "Calculates the hyperbolic cosine of x (with java.lang.Math)."
  [^double x]
  (Math/cosh x))

(defn sinh
  "Calculates the hyperbolic sine of x (with java.lang.Math)."
  [^double x]
  (Math/sinh x))

(defn tanh
  "Calculates the hyperbolic tangens of x (with java.lang.Math)."
  [^double x]
  (Math/tanh x))

(defn hypot
  "Calculates the hypothenuse of x and y (Pythagoras) (with java.lang.Math)."
  [^double x ^double y]
  (Math/hypot x y))

(defn deg-to-rad
  "Converts degrees to radians (with java.lang.Math)."
  [^double deg]
  (Math/toRadians deg))

(defn rad-to-deg
  "Converts radians to degrees (with java.lang.Math)."
  [^double rad]
  (Math/toDegrees rad))


;;
;; native implemented functions
;;
(defn sqr
  "Calculates the square of x."
  [x]
  (* x x))

(defn cube
  "Calculates the cube of x."
  [x]
  (* x x x))

(defn avg
  "Calculates the avarage of x and y or of the values of coll."
  ([x y]
   (/ (+ x y) 2))
  ([coll]
   (/ (reduce + 0 coll) (count coll))))

(defn factorial
  "Calculates the factorial of x."
  [^long x]
  (loop [curr (long x) fact (long 1)]
    (cond
      (<= curr 0) 0
      (= curr 1) fact
      :default (recur (dec curr) (long (*' curr fact))))))

(defn fibonacci
  "Calculates the fibonacci number of x."
  [^long x]
  (loop [a (long 1) b (long 0) cnt (long x)]
    (cond
      (= cnt 0) b
      :default (recur (long (+' a b)) (long a) (long (dec cnt))))))

;;
;; special trigonometric functions
;;
(defn hav
  "Calculates the haversine function of the angle a."
  [x]
  (sqr (sin (/ x 2))))

(defn ahav
  "Calculates the arc haversine function of the value v."
  [x]
  (* 2 (asin (sqrt x))))

;;
;; special functions
;;
(defn- tau-erf
  [x]
  (let [t (/ 1
             (+ 1 (* 1/2 (abs x))))]
    (- 1 (* t (exp (+ (* -1 x x)
                      -1.26551223
                      (*  1.00002368 t)
                      (*  0.37409196 t t)
                      (*  0.09678418 t t t)
                      (* -0.18628806 t t t t)
                      (*  0.27886807 t t t t t)
                      (* -1.13520398 t t t t t t)
                      (*  1.48851587 t t t t t t t)
                      (* -0.82215223 t t t t t t t t)
                      (*  0.17087277 t t t t t t t t t)))))))

(defn erf
  "Calculates the gaussian error function."
  [x]
  (let [z (tau-erf x)]
    (if (>= x 0)
      z
      (* -1 z))))

(defn erfc
  "Calculates the complementary gaussian error function."
  [x])
  ; TODO implement


