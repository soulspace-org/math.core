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
(ns org.soulspace.math.gradient
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.math.matrix :as mm]
            [org.soulspace.math.vector :as mv]))

#?(:clj
   (set! *warn-on-reflection* true))

;;
;; Functions for gradients and gradient descent
;;
(def step-sizes [100 10 1 0.1 0.01 0.001 0.0001 0.00001])

(defn safe-apply
  "Safely applies f so that it returns infinity whenever f returns an error."
  [f & args]
  (try
    (apply f args)
    #?(:clj
       (catch Exception e
         Double/POSITIVE_INFINITY))
    #?(:cljs
       (catch js/Error e
        Double/POSITIVE_INFINITY))
    ))

(defn safe-fn
  "Returns a function that is the same as f except that it returns infinity whenever f returns an error."
  [f]
  (fn [& args]
    (try
      (apply f args)
      #?(:clj
         (catch Exception e
           Double/POSITIVE_INFINITY))
      #?(:cljs
         (catch js/Error e
           Double/POSITIVE_INFINITY)))))

(defn partial-difference-quotient
  "Calculates the i-th partial difference quotient of f at v."
  [f v i h]
  (let [w (for [[j v-j] (map-indexed v)]
            (if (= i j)
              (+ v-j h)
              v-j))]
    (/ (- (f w) (f v))
       h)))

(defn estimate-gradient
  "Estimates the gradient of f at v."
  ([f v]
   (estimate-gradient f v 0.0000001))
  ([f v h]
   (mapv #(partial-difference-quotient f v % h) (range (count v)))))

(defn step
  "Returns the vector v moved step-size in direction."
  [v direction step-size]
  (for [[v-i direction-i] (map vector v direction)]
    (+ v-i (* step-size direction-i))))

(defn negated-fn
  "Returns a function that calculates the negated value (multiplied with -1) of f."
  [f]
  (fn [& args]
    (* -1 (apply f args))))

(defn negated-all-fn
  "Returns a function that calculates the negated values (multiplied with -1) of f for functions returning a sequence of values."
  [f]
  (fn [& args]
    (map #(* -1 %) (apply f args))))

(defn minimize-batch
  "Calculates ."
  ([target-fn gradient-fn theta-0]
   (minimize-batch target-fn gradient-fn theta-0 m/default-epsilon))
  ([target-fn gradient-fn theta-0 tolerance]
   (let [target-fn (safe-fn target-fn)]
     (loop [theta theta-0
            value (target-fn theta)]
       (let [gradient (gradient-fn theta)
             next-thetas (map #(step theta gradient (* -1 %)) step-sizes)
             next-theta (apply min-key target-fn next-thetas)
             next-value (target-fn next-theta)]
         (if (< (m/abs (- value next-value)) tolerance)
           theta
           (recur next-theta next-value)))))))

(defn maximize-batch
  "Calculates the theta that minimizes the target function by gradient descent."
  ([target-fn gradient-fn theta-0]
   (maximize-batch target-fn gradient-fn theta-0 m/default-epsilon))
  ([target-fn gradient-fn theta-0 tolerance]
   (minimize-batch (negated-fn target-fn) (negated-all-fn gradient-fn) theta-0 tolerance)))

(defn minimize-stochastic
  "Calculates ."
  ([target-fn gradient-fn x y theta-0]
   (minimize-stochastic target-fn gradient-fn x y theta-0 0.01))
  ([target-fn gradient-fn x y theta-0 alpha-0]))
    ; TODO implement
    ; use (shuffle coll)

(defn maximize-stochastic
  "Calculates ."
  ([target-fn gradient-fn x y theta-0]
   (maximize-stochastic target-fn gradient-fn x y theta-0 0.01))
  ([target-fn gradient-fn x y theta-0 alpha-0]
   (minimize-stochastic (negated-fn target-fn) (negated-all-fn gradient-fn) x y theta-0 alpha-0)))

;
; test gradient descent
;
(defn sum-of-squares-gradient
  "Calculates the gradient of the sum of squares of v."
  [v]
  (mapv #(* 2 %) v))

(defn minimize-sum-of-squares
  "Calculates the minimization of the sum of squares."
  [start-v]
  (loop [v start-v]
    (let [gradient (sum-of-squares-gradient v)
          next-v (step v gradient -0.01)]
      (if (< (mv/distance next-v v) 0.0000001)
        v
        (recur next-v)))))

;
; dimensional reduction via gradient descent
;
(def direction
  "Returns the direction of the vector w (which is w normalized to length 1)"
  mv/normalize)

(defn directional-variance-i
  "Calculates the variance of the row x-i in the direction of w."
  [x-i w]
  (m/sqr (mv/dot-product x-i (direction w))))

(defn directional-variance
  "Calculates the variance of the matrix m in the direction of w."
  [m w]
  (reduce + (map #(directional-variance-i % w) m)))

(defn directional-variance-gradient-i
  "Calculates the contribution of row x-i to the gradient of the variance in direction w."
  [x-i w]
  (let [projection-length (mv/dot-product x-i (direction w))]
    (mapv #(* 2 projection-length %) x-i)))

(defn directional-variance-gradient
  "Calculates the contribution of the matrix m to the gradient of the variance in direction w."
  [m w]
  (mapv #(mv/add (directional-variance-gradient-i % w)) m))
