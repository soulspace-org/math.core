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
  [^double p]
  (if (< (rand) p)
    1
    0))

(defn binomial
  "Calculates the binomial(n, p) random variable."
  [n ^double p]
  (reduce + 0 (take n (repeatedly #(bernoulli p)))))

(defn binomial-approximation-by-normal
  "Approximates mu and sigma corresponding to a binomial(n, p) variable."
  ^double [^double n ^double p]
  [(* p n) (m/sqrt (* p (- 1 p) n))])

(defn uniform-pdf
  "Calculates the probability density function for the uniform distribution."
  [^double x]
  (if (and (>= x 0) (< x 1))
    1
    0)

 (defn uniform-cdf
   "Calculates the cumulative distribution function for the uniform distribution."
   [^double x]
   (cond
     (< x 0) 0
     (< x 1) 1
     :else 1)))

(defn normal-pdf
  "Calculates the probability density function for the normal distribution."
  (^double [^double x]
   (normal-pdf x 0 1))
  (^double [^double x [^double mu ^double sigma]]
   (normal-pdf x mu sigma))
  (^double [^double x ^double mu ^double sigma]
   (/ (m/exp (* -1 (/ (m/sqr (- x mu))
                    (* 2 sigma sigma))))
      (m/sqrt (* 2 m/pi)))))

(defn normal-cdf
  "Calculates the cumulative distribution function for the normal distribution."
  (^double [^double x]
   (normal-cdf x 0 1))
  (^double [^double x [^double mu ^double sigma]]
   (normal-cdf x mu sigma))
  (^double [^double x ^double mu ^double sigma]
   (/ (+ 1 (m/erf (/ (- x mu)
                   (* (m/sqrt 2) sigma))))
      2)))

(defn inverse-normal-cdf-with-tolerance
  "Calculates the inverse of the cumulative distribution function for the normal distribution. Approximates the value with binary search."
  (^double [^double p ^double e]
   (mm/search-value normal-cdf p -10.0 10.0 e))
  (^double [^double p ^double mu ^double sigma ^double e]
   (+ mu (* sigma (inverse-normal-cdf-with-tolerance p e)))))

(defn inverse-normal-cdf
  "Calculates the inverse of the cumulative distribution function for the normal distribution. Approximates the value with binary search."
  (^double [^double p]
   (inverse-normal-cdf-with-tolerance p 0.00001))
  (^double [^double p [^double mu ^double sigma]]
   (inverse-normal-cdf p mu sigma))
  (^double [^double p ^double mu ^double sigma]
   (+ mu (* sigma (inverse-normal-cdf-with-tolerance p 0.00001)))))

(def normal-probability-below
  "Calculates the probability that the variable is below the given value x (which is exactly what the cumulative distribution function gives us)."
  normal-cdf)

(defn normal-probability-above
  "Calculates the probability that the variable is below the given value x."
  (^double [^double x]
   (normal-probability-above x 0 1))
  (^double [^double x [^double mu ^double sigma]]
   (normal-probability-above x mu sigma))
  (^double [^double x ^double mu ^double sigma]
   (- 1 (normal-cdf x mu sigma))))

(defn normal-probability-between
  "Calculates the probability that the variable is below the given value x."
  (^double [^double low ^double high]
   (normal-probability-between low high 0 1))
  (^double [^double low ^double high [^double mu ^double sigma]]
   (normal-probability-between low high mu sigma))
  (^double [^double low ^double high ^double mu ^double sigma]
   (- (normal-cdf high mu sigma) (normal-cdf low mu sigma))))

(defn normal-probability-outside
  "Calculates the probability that the variable is below the given value x."
  (^double [^double low ^double high]
   (normal-probability-outside low high 0 1))
  (^double [^double low ^double high [^double mu ^double sigma]]
   (normal-probability-outside low high mu sigma))
  (^double [^double low ^double high ^double mu ^double sigma]
   (- 1 (normal-probability-between low high mu sigma))))

(defn normal-upper-bound
  "Calculates the x for which P(X <= x) = probability."
  (^double [^double probability]
   (normal-upper-bound probability 0 1))
  (^double [^double probability [^double mu ^double sigma]]
   (normal-upper-bound probability mu sigma))
  ([^double probability ^double mu ^double sigma]
   (inverse-normal-cdf probability mu sigma)))

(defn normal-lower-bound
  "Calculates the x for which P(X >= x) = probability."
  (^double [^double probability]
   (normal-lower-bound probability 0 1))
  (^double [^double probability [^double mu ^double sigma]]
   (normal-lower-bound probability mu sigma))
  (^double [^double probability ^double mu ^double sigma]
   (inverse-normal-cdf (- 1 probability) mu sigma)))

(defn normal-symmetric-bounds
  "Calculates the symmetric bounds around the mean."
  (^double [^double probability]
   (normal-symmetric-bounds probability 0 1))
  (^double [^double probability [^double mu ^double sigma]]
   (normal-symmetric-bounds probability mu sigma))
  (^double [^double probability ^double mu ^double sigma]
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
  (^double [^double x]
   (two-sided-p-value x 0 1))
  (^double [^double x [^double mu ^double sigma]]
   (two-sided-p-value x mu sigma))
  (^double [^double x ^double mu ^double sigma]
   (if (< x mu)
     (* 2 (normal-probability-below x mu sigma))
     (* 2 (normal-probability-above x mu sigma)))))
