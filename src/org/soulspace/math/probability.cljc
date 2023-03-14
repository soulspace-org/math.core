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
(ns org.soulspace.math.probability
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.math.methods :as mm]))

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Probability functions
;;;
;;; implements algorithms e.g. from
;;;   Grus, Joel; Data Science from Scratch; O'Reilly
;;;
(defn bernoulli
  "Calculates the bernulli(p) random variable. Returns 1 with a probability of p and 0 with a probability of (1 - p)."
  [p]
  (if (< (rand) p)
    1
    0))

(defn binomial
  "Calculates the binomial(n, p) random variable."
  [n p]
  (reduce + 0 (take n (repeatedly #(bernoulli p)))))

(defn binomial-approximation-by-normal
  "Approximates mu and sigma corresponding to a binomial(n, p) variable."
  [n p]
  [(* p n) (m/sqrt (* p (- 1 p) n))])

(defn uniform-pdf
  "Calculates the probability density function for the uniform distribution."
  [x]
  (if (and (>= x 0) (< x 1))
    1
    0)

 (defn uniform-cdf
   "Calculates the cumulative distribution function for the uniform distribution."
   [x]
   (cond
     (< x 0) 0
     (< x 1) 1
     :else 1)))

(defn normal-pdf
  "Calculates the probability density function for the normal distribution."
  ([x]
   (normal-pdf x 0 1))
  ([x [mu sigma]]
   (normal-pdf x mu sigma))
  ([x mu sigma]
   (/ (m/exp (* -1 (/ (m/sqr (- x mu))
                    (* 2 sigma sigma))))
      (m/sqrt (* 2 m/pi)))))

(defn normal-cdf
  "Calculates the cumulative distribution function for the normal distribution."
  ([x]
   (normal-cdf x 0 1))
  ([x [mu sigma]]
   (normal-cdf x mu sigma))
  ([x mu sigma]
   (/ (+ 1 (m/erf (/ (- x mu)
                   (* (m/sqrt 2) sigma))))
      2)))

(defn inverse-normal-cdf-with-tolerance
  "Calculates the inverse of the cumulative distribution function for the normal distribution. Approximates the value with binary search."
  ([p e]
   (mm/search-value normal-cdf p -10.0 10.0 e))
  ([p mu sigma e]
   (+ mu (* sigma (inverse-normal-cdf-with-tolerance p e)))))

(defn inverse-normal-cdf
  "Calculates the inverse of the cumulative distribution function for the normal distribution. Approximates the value with binary search."
  ([p]
   (inverse-normal-cdf-with-tolerance p 0.00001))
  ([p [mu sigma]]
   (inverse-normal-cdf p mu sigma))
  ([p mu sigma]
   (+ mu (* sigma (inverse-normal-cdf-with-tolerance p 0.00001)))))

(def normal-probability-below
  "Calculates the probability that the variable is below the given value x (which is exactly what the cumulative distribution function gives us)."
  normal-cdf)

(defn normal-probability-above
  "Calculates the probability that the variable is below the given value x."
  ([x]
   (normal-probability-above x 0 1))
  ([x [mu sigma]]
   (normal-probability-above x mu sigma))
  ([x mu sigma]
   (- 1 (normal-cdf x mu sigma))))

(defn normal-probability-between
  "Calculates the probability that the variable is below the given value x."
  ([low high]
   (normal-probability-between low high 0 1))
  ([low high [mu sigma]]
   (normal-probability-between low high mu sigma))
  ([low high mu sigma]
   (- (normal-cdf high mu sigma) (normal-cdf low mu sigma))))

(defn normal-probability-outside
  "Calculates the probability that the variable is below the given value x."
  ([low high]
   (normal-probability-outside low high 0 1))
  ([low high [mu sigma]]
   (normal-probability-outside low high mu sigma))
  ([low high mu sigma]
   (- 1 (normal-probability-between low high mu sigma))))

(defn normal-upper-bound
  "Calculates the x for which P(X <= x) = probability."
  ([probability]
   (normal-upper-bound probability 0 1))
  ([probability [mu sigma]]
   (normal-upper-bound probability mu sigma))
  ([probability mu sigma]
   (inverse-normal-cdf probability mu sigma)))

(defn normal-lower-bound
  "Calculates the x for which P(X >= x) = probability."
  ([probability]
   (normal-lower-bound probability 0 1))
  ([probability [mu sigma]]
   (normal-lower-bound probability mu sigma))
  ([probability mu sigma]
   (inverse-normal-cdf (- 1 probability) mu sigma)))

(defn normal-symmetric-bounds
  "Calculates the symmetric bounds around the mean."
  ([probability]
   (normal-symmetric-bounds probability 0 1))
  ([probability [mu sigma]]
   (normal-symmetric-bounds probability mu sigma))
  ([probability mu sigma]
   (let [tail-probability (/ (- 1 probability) 2)]
     [(normal-lower-bound tail-probability mu sigma) (normal-upper-bound tail-probability mu sigma)])))

(def upper-p-value
  "Calculates the probability for seeing the value x."
  normal-probability-above)

(def lower-p-value
  "Calculates the probability for seeing the value x."
  normal-probability-below)

(defn two-sided-p-value
  "Calculates the probability for seeing the value x."
  ([x]
   (two-sided-p-value x 0 1))
  ([x [mu sigma]]
   (two-sided-p-value x mu sigma))
  ([x mu sigma]
   (if (< x mu)
     (* 2 (normal-probability-below x mu sigma))
     (* 2 (normal-probability-above x mu sigma)))))
