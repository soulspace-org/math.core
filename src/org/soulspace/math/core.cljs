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
;;; JavaScript Math implementations for ClojureScript
;;;
(def default-epsilon "Default tolerance (epsilon)." 0.00001)
(def default-dx "Default step size (delta x)." 0.0000001)

(def PI (.-PI js/Math))
(def E (.-E js/Math))
(def LN2 (.-LN2 js/Math))
(def DOUBLE-PI (* 2 PI))
(def HALF-PI (/ PI 2))
;   (def ln10 (.-LN10 js/Math))
;   (def log2e (.-LOG2E js/Math))
;   (def log10e (.-LOG10E js/Math))
;   (def sqrt1-2 (.-SQRT1_2 js/Math))
;   (def sqrt2 (.-SQRT2 js/Math))

(defn abs
  "Calculates the absolute value of x."
  [x]
  (.abs js/Math x))

(defn sign
  "Calculates the sign of x."
  [x]
  (.signum js/Math x))

(defn floor
  "Calculates x, rounded downwards to the nearest integer."
  [x]
  (.floor js/Math x))

(defn ceil
  "Calculates x, rounded upwards to the nearest integer."
  [x]
  (.ceil js/Math x))

(defn sqrt
  "Calculates the square root of x."
  [x]
  (.sqrt js/Math x))

(defn cbrt
  "Calculates the cubic root of x."
  [x]
  (.cbrt js/Math x))

(defn pow
  "Calculates the value of x to the power of y."
  [x y]
  (.pow js/Math x y))

(defn exp
  "Calculates the value of E^x."
  [x]
  (.exp js/Math x))

(defn expm1
  "Calulates e to the power of x minus 1."
  [x]
  (.expm1 js/Math x))

(defn log
  "Calculates the natural logarithm (base E) of x."
  [x]
  (.log js/Math x))

(defn log-with-base
  "Calculates the logarithm with base b of x."
  [b x]
  (/ (log x) (log b)))

(defn log10
  "Calculates the logarithm with base 10 of x (with java.lang.Math)."
  [^double x]
  (.log10 js/Math x))

(defn alog10
  "Calculates the inverse of the logarithm with base 2 of x (with java.lang.Math)."
  [^double x]
  (.pow js/Math 10 x))

(defn log2
  "Calculates the logarithm with base 2 of x (with java.lang.Math)."
  [^double x]
  (.log2 js/Math x))

(defn alog2
  "Calculates the inverse of the logarithm with base 2 of x (with java.lang.Math)."
  [^double x]
  (.pow js/Math 2 x))

(defn log1p
  "Calculates the natural logarithm of the sum of x and 1."
  [^double x]
  (.log1p js/Math x))

;;
;; trigonometric functions
;;
(defn sin
  "Calculates the sine of x (x is in radians)."
  [x]
  (.sin js/Math x))

(defn cos
  "Calculates the cosine of x (x is in radians)."
  [x]
  (.cos js/Math x))

(defn tan
  "Calculates the tangens of x (x is in radians)."
  [x]
  (.tan js/Math x))

(defn asin
  "Calculates the arcsine of x (x is in radians)."
  [x]
  (.asin js/Math x))

(defn acos
  "Calculates the arccosine of x (x is in radians)."
  [x]
  (.acos js/Math x))

(defn atan
  "Calculates the arctangent of x as a numeric value between -PI/2 and PI/2 radians."
  [x]
  (.atan js/Math x))

(defn atan2
  "Calculates the arctangent of the quotient of its arguments."
  [x y]
  (.atan2 js/Math x y))

(defn sinh
  "Calculates the hyperbolic sine of x (x is in radians)."
  [x]
  (.sinh js/Math x))

(defn cosh
  "Calculates the hyperbolic cosine of x (x is in radians)."
  [x]
  (.cosh js/Math x))

(defn tanh
  "Calculates the  hyperbolic tangens of x (x is in radians)."
  [x]
  (.tanh js/Math x))

(defn hypot
  "Calculates the hypothenuse of x and y (Pythagoras)."
  [x y]
  (.hypot js/Math x))

(defn rad-to-deg
  "Converts radians to degrees"
  [rad]
  (* (/ 180 PI) rad))

(defn deg-to-rad
  "Converts degrees to radians"
  [deg]
  (* deg (/ PI 180)))


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
      :default (recur (dec curr) (long (* curr fact))))))

(defn fibonacci
  "Calculates the fibonacci number of x."
  [^long x]
  (loop [a (long 1) b (long 0) cnt (long x)]
    (cond
      (= cnt 0) b
      :default (recur (long (+ a b)) (long a) (long (dec cnt))))))

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


