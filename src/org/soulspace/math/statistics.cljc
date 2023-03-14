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
(ns org.soulspace.math.statistics
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.math.matrix :as mm]
            [org.soulspace.math.vector :as mv]))

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Statistics functions
;;;

; same as avg
(defn geometric-average
  "Returns the geometric average"
  [coll]
  (m/pow (* coll) (- (count coll))))

(def mean
  "Returns the mean of the values of the coll."
  m/avg)

(def geometric-average
  "Returns the geometric average (mean) of the values of the coll."
  m/avg)

(defn harmonic-average
  "Returns the harmonic average of the values of the coll."
  [coll]
  (reduce + 0 (map #(/ 1 %) coll)))

(defn square-average
  "Returns the square average of the values of the coll."
  [coll]
  (m/sqrt (/ (reduce + 0 (map m/sqr coll))
             (count coll))))

(defn cubic-average
  "Returns the cubic average of the values of the coll."
  [coll]
  (m/cbrt (/ (reduce + 0 (map m/cube coll))
             (count coll))))

(defn de-mean
  "Returns a collection with the mean substacted from each value of input coll."
  [coll]
  (let [x-mean (mean coll)]
    (map #(- % x-mean) coll)))

(defn quantile
  "Returns the q quantile"
  [q coll]
  (let [x  (* (count coll) q)]
    (if (integer? x)
      (/ (+ (nth coll (- x 1)) (nth coll x))
         2)
      (nth coll (- (m/ceil x) 1)))))

(defn median
  "Returns the median / second quartile"
  [coll]
  (quantile (/ 1 2) coll))

(defn quartile1
  "Returns the first quartile"
  [coll]
  (quantile (/ 1 4) coll))

(defn quartile2
  "Returns the second quartile / median"
  [coll]
  (median coll))

(defn quartile3
  "Returns the third quartile"
  [coll]
  (quantile (/ 3 4) coll))

(defn variance
  "Returns the biased sample variance (n)"
  [coll]
  (let [mu (mean coll)
        n (count coll)]
    (/ (reduce + 0 (map #(m/sqr (- % mu)) coll))
       n)))

(defn unbiased-variance
  "Returns the unbiased sample variance (n-1)"
  [coll]
  (let [mu (mean coll)
        n (count coll)]
    (/ (reduce + 0 (map #(m/sqr (- % mu)) coll))
       (- n 1))))

(defn covariance
  "Returns the biased sample covariance (n)"
  [coll1 coll2]
  (let [mu1 (mean coll1)
        mu2 (mean coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       n)))

(defn unbiased-covariance
  "Returns the unbiased sample covariance (n-1)"
  [coll1 coll2]
  (let [mu1 (mean coll1)
        mu2 (mean coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       (- n 1))))

(defn deviation
  "Returns the biased sample deviation (n)"
  [coll]
  (m/sqrt (variance coll)))

(defn unbiased-deviation
  "Returns the unbiased sample deviation (n-1)"
  [coll]
  (m/sqrt (unbiased-variance coll)))

(defn correlation-coefficient
  "Returns the correlation coefficient"
  [coll1 coll2]
  (/ (covariance coll1 coll2)
     (* (deviation coll1) (deviation coll2))))

(defn linear-regression
  "Returns a vector [a b] of the linear regession coefficients for the equation y = ax + b."
  [coll1 coll2]
  (let [mu1 (mean coll1)
        mu2 (mean coll2)
        a (/ (covariance coll1 coll2) (variance coll1))]
    [a (- mu2 (* a mu1))]))

(defn q-value
  "Multiplies the count of coll with q."
  [q coll]
  (* (count coll) q))

;
; hypothesis tests
;
(defn- estimated-parameters
  "Calculates the probability and standard deviation for a given test outcome of n out of N."
  ([[N n]]
   (estimated-parameters N n))
  ([N n]
   (let [p (/ n N)]
     [p (m/sqrt (/ (* p (- 1 p)) N))])))

(defn a-b-test-statistic
  "Calculates the statistic for the hypothesis, that p-a and p-b are the same, given the outcomes for test a and b."
  [N-a n-a N-b n-b]
  (let [[p-a sigma-a] (estimated-parameters N-a n-a)
        [p-b sigma-b] (estimated-parameters N-b n-b)]
    (/ (- p-b p-a)
       (m/sqrt (+ (* sigma-a sigma-a) (* sigma-b sigma-b))))))

;
; rescaling of data matrices
;
(defn scale
  "Returns the mean vector and the unbiased standard deviation vector for the colums of the matrix."
  [m]
  (let [cols (mm/column-vectors m)]
    [(mapv mean cols) (mapv unbiased-deviation cols)]))

(defn rescale
  "Rescales the matrix m to have a mean of 0 and an unbiased standard deviation of 1."
  [m]
  (let [[rows cols] (mm/shape m)
        [means stdevs] (scale m)
        rescaled (fn [i j]
                   (if (> (stdevs j) 0)
                     (/ (- (mm/element m i j) (means j))
                        (stdevs j))
                     (mm/element m i j)))]
    (mm/build-matrix rows cols rescaled)))

(defn de-mean-matrix
  "Returns a matrix with the column mean substacted from each value of input matrix m."
  [m]
  (let [[rows cols] (mm/shape m)
        [means _] (scale m)]
    (mm/build-matrix rows cols (fn [i j] (- (mm/element m i j) (means j))))))

