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
  (:require [org.soulspace.math.core :as mc]
            [org.soulspace.math.matrix :as mm]))
#?(:clj  (require '[clojure.math :as m])
   :cljs (require '[cljs.math :as m]))

;;;
;;; Statistics functions
;;;
(comment
  ; same as m/avg
  (defn geometric-average
    "Returns the geometric average of the values of the collection `coll`."
    ^double [coll]
    (m/pow (* coll) (- (count coll))))
  )

(def geometric-average
  "Returns the geometric average (mean) of the values of the collection `coll`."
  mc/avg)

(def mean
  "Returns the mean of the values of the collection `coll`."
  mc/avg)

(defn harmonic-average
  "Returns the harmonic average of the values of the collection `coll`."
  ^double [coll]
  (reduce + 0 (map #(/ 1 %) coll)))

(defn square-average
  "Returns the square average of the values of the collection `coll`."
  ^double [coll]
  (m/sqrt (/ (reduce + 0 (map mc/sqr coll))
             (count coll))))

(defn cubic-average
  "Returns the cubic average of the values of the collection `coll`."
  ^double [coll]
  (m/cbrt (/ (reduce + 0 (map mc/cube coll))
             (count coll))))

(defn de-mean
  "Returns a collection with the mean substacted from each value of input collection `coll`."
  [coll]
  (let [x-mean (mean coll)]
    (map #(- % x-mean) coll)))

(defn quantile
  "Returns the `q` quantile for the collection `coll`."
  ^double [q coll]
  (let [x  (* (count coll) q)]
    (if (integer? x)
      (/ (+ (nth coll (- x 1)) (nth coll x))
         2)
      (nth coll (- (m/ceil x) 1)))))

(defn median
  "Returns the median / second quartile for the collection `coll`."
  ^double [coll]
  (quantile (/ 1 2) coll))

(defn quartile1
  "Returns the first quartile for the collection `coll`."
  ^double [coll]
  (quantile (/ 1 4) coll))

(defn quartile2
  "Returns the second quartile / median for the collection `coll`."
  ^double [coll]
  (median coll))

(defn quartile3
  "Returns the third quartile for the collection `coll`."
  ^double [coll]
  (quantile (/ 3 4) coll))

(defn variance
  "Returns the biased sample variance (n) for the collection `coll`."
  ^double [coll]
  (let [mu (mean coll)
        n (count coll)]
    (/ (reduce + 0 (map #(mc/sqr (- % mu)) coll))
       n)))

(defn unbiased-variance
  "Returns the unbiased sample variance (n-1) for the collection `coll`."
  ^double [coll]
  (let [mu (mean coll)
        n (count coll)]
    (/ (reduce + 0 (map #(mc/sqr (- % mu)) coll))
       (- n 1))))

(defn covariance
  "Returns the biased sample covariance (n) for the collections `coll1` and `coll2`."
  [coll1 coll2]
  (let [mu1 (mean coll1)
        mu2 (mean coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       n)))

(defn unbiased-covariance
  "Returns the unbiased sample covariance (n-1) for the collections `coll1` and `coll2`."
  [coll1 coll2]
  (let [mu1 (mean coll1)
        mu2 (mean coll2)
        n (count coll1)]
    (/ (reduce + 0 (map #(* (- %1 mu1) (- %2 mu2)) coll1 coll2))
       (- n 1))))

(defn deviation
  "Returns the biased sample deviation (n) for the collection `coll`."
  ^double [coll]
  (m/sqrt (variance coll)))

(defn unbiased-deviation
  "Returns the unbiased sample deviation (n-1) for the collection `coll`."
  ^double [coll]
  (m/sqrt (unbiased-variance coll)))

(defn correlation-coefficient
  "Returns the correlation coefficient for the collections `coll1` and `coll2`."
  ^double [coll1 coll2]
  (/ (covariance coll1 coll2)
     (* (deviation coll1) (deviation coll2))))

(defn linear-regression
  "Returns a vector [a b] of the linear regession coefficients for the equation y = ax + b for the collections `coll1` and `coll2`."
  [coll1 coll2]
  (let [mu1 (mean coll1)
        mu2 (mean coll2)
        a (/ (covariance coll1 coll2) (variance coll1))]
    [a (- mu2 (* a mu1))]))

(defn q-value
  "Multiplies the count of `coll` with `q`."
  [q coll]
  (* (count coll) q))

;;
;; hypothesis tests
;;
(defn- estimated-parameters
  "Calculates the probability and standard deviation for a given test outcome of `n` out of `N`."
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

;;
;; rescaling of data matrices
;;
(defn scale
  "Returns the mean vector and the unbiased standard deviation vector for the colums of the matrix `m`."
  [m]
  (let [cols (mm/column-vectors m)]
    [(mapv mean cols) (mapv unbiased-deviation cols)]))

(defn rescale
  "Rescales the matrix `m` to have a mean of 0 and an unbiased standard deviation of 1."
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
  "Returns a matrix with the column mean substacted from each value of input matrix `m`."
  [m]
  (let [[rows cols] (mm/shape m)
        [means _] (scale m)]
    (mm/build-matrix rows cols (fn [i j] (- (mm/element m i j) (means j))))))
