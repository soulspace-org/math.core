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

#?(:clj (ns org.soulspace.math.core
          (:require [clojure.math :as m]))
   :cljs (ns org.soulspace.math.core
           (:require [cljs.math :as m])))

(def default-epsilon "Default tolerance (epsilon)." 0.00001)
(def default-dx "Default step size (delta x)." 0.0000001)

(def ^:const LN2 (m/log 2))
(def ^:const DOUBLE-PI (* 2 m/PI))
(def ^:const HALF-PI (/ m/PI 2))

(defn log-with-base
  "Calculates the logarithm of `x` with base `b`."
  [b x]
  (/ (m/log x) (m/log b)))

(defn alog10
  "Calculates the inverse of the logarithm with base 2 of `x`."
  [x]
  (m/pow 10 x))

(defn log2
  "Calculates the logarithm with base 2 of `x`."
  [x]
  (/ (m/log x) LN2))

(defn alog2
  "Calculates the inverse of the logarithm with base 2 of `x`."
  [x]
  (m/pow 2 x))

; Trigonometrical functions

(defn atan2
  "Calculates the arc tangens of `a`/`b`. Returns the result in the correct quadrant."
  [a b]
  (let [r (m/atan (/ a b))] ; TODO handle b = 0 case
    (cond
      (< b 0) (if (< a 0)
                (- r m/PI) ; adjust quadrant
                (+ r m/PI)) ; adjust quadrant
      :else r)))

;;
;; native implemented functions
;;
(defn sqr
  "Calculates the square of `x`."
  [x]
  (* x x))

(defn cube
  "Calculates the cube of `x`."
  [x]
  (* x x x))

(defn avg
  "Calculates the avarage of `x` and `y` or of the values of `coll`."
  ([x y]
   (/ (+ x y) 2))
  ([coll]
   (/ (reduce + 0 coll) (count coll))))

(defn factorial
  "Calculates the factorial of `x`."
  ^long [^long x]
  (loop [curr (long x) fact (long 1)]
    (cond
      (<= curr 0) 0
      (= curr 1) fact
      :else #?(:clj (recur (dec curr) (long (*' curr fact)))
                  :cljs (recur (dec curr) (long (* curr fact)))))))

(defn fibonacci
  "Calculates the fibonacci number of `x`."
  ^long [^long x]
  (loop [a (long 1) b (long 0) cnt (long x)]
    (if (= cnt 0)
      b
      #?(:clj (recur (long (+' a b)) (long a) (long (dec cnt)))
         :cljs (recur (long (+ a b)) (long a) (long (dec cnt)))))))

;;
;; special trigonometric functions
;;
(defn hav
  "Calculates the haversine function of the angle `x` (given in rad)."
  [x]
  (sqr (m/sin (/ x 2))))

(defn ahav
  "Calculates the arc haversine function of the value `x`."
  [x]
  (* 2 (m/asin (m/sqrt x))))

;;
;; special functions
;;
(defn- tau-erf
  [x]
  (let [t (/ 1
             (+ 1 (* 1/2 (abs x))))]
    (- 1 (* t (m/exp (+ (* -1 x x)
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
  "Calculates the gaussian error function of `x`."
  [x]
  (let [z (tau-erf x)]
    (if (>= x 0)
      z
      (* -1 z))))

#_(defn erfc
  "Calculates the complementary gaussian error function of `x`."
  [x]
  ; TODO implement
  )


