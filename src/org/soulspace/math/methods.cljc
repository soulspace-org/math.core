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
(ns org.soulspace.math.methods
  (:require [org.soulspace.math.core :as m]))

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Mathemathical algorithms and methods
;;;

;;
;; Number Theory
;;

(defn gcd
  "Calculates the greatest common divisor of x and y"
  [x y]
  (if (= y 0)
    x
    (recur y (rem x y))))

; TODO this fn is subject to a StackOverflowException, find a better implementation.
(defn primes
  "Returns a lazy sequence of primes."
  ([]
   (primes (iterate inc 2)))
  ([coll]
   (cons (first coll)
         (lazy-seq (primes (filter #(not= 0 (mod % (first coll))) (rest coll)))))))

;;
;;
;;  
(defn exp
  "Calculates the exponential function."
  [b n]
  (cond
    (= n 0) 1
    (even? n) (m/sqr (exp b (/ n 2)))
    :default (* b (exp b (- n 1)))))

(defn round-up
  "Rounds a value."
  [x n]
  (/ (m/floor (+ (* x (exp 10 n)) 0.5)) (exp 10 n)))

(defn close-enough?
  "Checks for a difference smaller than epsilon"
  ([x y]
   (close-enough? x y m/default-epsilon))
  ([x y epsilon]
   (< (m/abs (- x y)) epsilon)))

(defn average-damp
  "Returns a function with average dampening for the given function."
  [f]
  (fn [x] (m/avg x (f x))))

(defn sum
  "Calculates the sum of term between a and b with the step function nxt."
  [term a nxt b]
  ; TODO refactor to loop/recur
  (if (> a b)
    0
    (+ (term a)
       (sum term (nxt a) nxt b))))

(defn prod
  "Calculates the product of term between a and b with the step function nxt."
  [term a nxt b]
  ; TODO refactor to loop/recur
  (if (> a b)
    0
    (* (term a)
       (prod term (nxt a) nxt b))))

;;
;; 
;;

(defn quadratic-roots
  "Returns the roots of the quadratic equation (a * x^2 + b * x + c = 0)."
  [a b c]
  ; minus is subject to catastrophic cancelling of significant digits in
  ; floating point calculations
  ; TODO: refactor for better numeric stability
  (let [d (- (* b b) (* 4 a c))]
    [(/ (+ (- b) (m/sqrt d)) (* 2 a))
     (/ (- (- b) (m/sqrt d)) (* 2 a))]))

;;
;; calculus/analysis
;;
(defn search-value
  "Searches for value by interval search."
  ([f v low high]
   (search-value f v low high m/default-epsilon))
  ([f v low high epsilon]
   (let [mid (m/avg low high)]
     (if (close-enough? low high epsilon)
         mid
         (let [v-test (f mid)]
           (cond
             (> v-test v) (recur f v low mid epsilon)
             (< v-test v) (recur f v mid high epsilon)
             :default mid))))))

(defn search-zero
  "Searches for zero by interval search."
  ([f neg-point pos-point]
   (search-zero f neg-point pos-point m/default-epsilon))
  ([f neg-point pos-point epsilon]
   (let [midpoint (m/avg neg-point pos-point)]
     (if (close-enough? neg-point pos-point epsilon)
         midpoint
         (let [test-value (f midpoint)]
           (cond
             (pos? test-value) (recur f neg-point midpoint epsilon)
             (neg? test-value) (recur f midpoint pos-point epsilon)
             :default midpoint))))))

(defn half-interval
  "Half interval method for the function f and values a and b."
  ([f a b]
   (half-interval f a b m/default-epsilon))
  ([f a b epsilon]
   (let [a-value (f a)
         b-value (f b)]
     (cond
       (and (neg? a-value) (pos? b-value)) (search-zero f a b epsilon)
       (and (neg? b-value) (pos? a-value)) (search-zero f b a epsilon)
       :else (throw (ex-info "The values are not of opposite signs." {:a-value a-value 
                                                                         :b-value b-value}))))))

(defn fixed-point
  "Calculates a fixed point of the function f."
  [f first-guess]
  (loop [guess first-guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (recur next-guess)))))

(defn integral
  "Calculates the integral of function f between a and b with dx."
  ([f a b]
   (integral f a b m/default-dx))
  ([f a b dx]
   (* (sum f (+ a (/ dx 2)) (partial + dx) b) dx)))

(defn difference-quotient
  "Calculates the difference quotient of the function f at x with the delta dx."
  ([f x]
   (difference-quotient f x m/default-dx))
  ([f x dx]
   (/ (- (f (+ x dx)) (f x))
      dx)))

(defn derivation
  "Returns a funtion that is the derivation of the function f."
  ([f]
   (derivation f m/default-dx))
  ([f dx]
   (fn [x]
     ; (difference-quotient f x dx)
     (/ (- (f (+ x dx)) (f x))
        dx))))

(defn newton-transform
  "Returns a function which is the newton transfomation of the given function f."
  ([f]
   (newton-transform f m/default-dx))
  ([f dx]
   (fn [x]
     (- x (/ (f x) ((derivation f dx) x))))))

(defn newton-method
  "Newton method for searching a root of the function f starting with guess."
  ([f guess]
   (newton-method f m/default-dx guess))
  ([f dx guess]
   (fixed-point (newton-transform f dx) guess)))
