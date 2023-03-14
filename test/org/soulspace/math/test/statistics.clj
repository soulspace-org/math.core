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
(ns org.soulspace.math.test.statistics
  (:require [clojure.test :refer :all]
            [org.soulspace.math.core :as m]
            [org.soulspace.math.statistics :refer :all]))

(deftest mean-test
  (is (== (mean [3 3 3 3 3]) 3))
  (is (== (mean [1 2 3 4 5]) 3))
  (is (== (mean [1 2 3 4]) 2.5))
  (is (== (mean [2 4 8 16]) 7.5)))

(deftest median-test
  (is (== (median [3 3 3 3 3]) 3))
  (is (== (median [1 2 3 4 5]) 3))
  (is (== (median [1 2 3 4]) 2.5))
  (is (== (median [2 4 8 16]) 6)))

(deftest variance-test
  (is (== (variance [3 3 3 3 3]) 0))
  (is (== (variance [1 2 3 4 5]) 2))
  (is (== (variance [1 2 3 4]) 1.25))
  (is (== (variance [2 4 8 16]) 115/4)))

(deftest deviation-test
  (is (== (deviation [3 3 3 3 3]) 0))
  (is (== (deviation [1 2 3 4 5]) (m/sqrt 2)))
  (is (== (deviation [1 2 3 4]) (m/sqrt 1.25)))
  (is (== (deviation [2 4 8 16]) (m/sqrt 115/4))))

(deftest covariance-test
  (is (= (covariance [1 2 3 4] [1 2 3 4]) 5/4)))

(deftest linear-regression-test
  (is (= (linear-regression [1 2 3 4] [2 3 4 5]) [1N 1N]))
  (is (= (linear-regression [1 2 3 4] [1 2 3 4]) [1N 0N]))
  (is (= (linear-regression [1 2 3 4] [-1 -2 -3 -4]) [-1N 0N])))

(deftest quantile-test
  (is (= (quantile 1/2 [1 2 3 4 5 6 7 8]))))
