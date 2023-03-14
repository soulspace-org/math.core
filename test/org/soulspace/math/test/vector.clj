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
(ns org.soulspace.math.test.vector
  (:require [clojure.test :refer :all]
            [org.soulspace.math.core :as m]
            [org.soulspace.math.vector :refer :all]))

(deftest add-test
  (are [x y] (= x y)
       [0] (add [0])
       [0] (add [0] [0])
       [0] (add [-1] [1])
       [2] (add [1] [1])
       [3] (add [1] [1] [1])
       [0 0] (add [0 0])
       [0 0] (add [0 0] [0 0])
       [0 0] (add [-1 0] [1 0])
       [2 0] (add [1 0] [1 0])
       [0 2] (add [0 1] [0 1])
       [3 0] (add [1 0] [1 0] [1 0])
       [0 3] (add [0 1] [0 1] [0 1])
       [3 0 0] (add [1 0 0] [1 0 0] [1 0 0])
       [0 3 0] (add [0 1 0] [0 1 0] [0 1 0])
       [3 3 3 3 3] (add [1 1 1 1 1] [1 1 1 1 1] [1 1 1 1 1])))

(deftest add-exception-test
  (is (thrown? Exception (add [1] [0 1])))
  (is (thrown? Exception (add [1 0 0 0] [0 1 0]))))

(deftest cross-product-test
  (are [x y] (= x y)
       [0 0 0] (cross-product [0 0 0] [0 0 0])
       [0 0 0] (cross-product [1 0 0] [1 0 0])
       [0 0 0] (cross-product [2 4 0] [2 4 0])
       [0 0 1] (cross-product [1 0 0] [0 1 0])
       [-1 0 0] (cross-product [0 0 1] [0 1 0])))

(deftest cross-product-exception-test
  (is (thrown? Exception (cross-product [1 0] [0 1])))
  (is (thrown? Exception (cross-product [1 0 0 0] [0 1 0 0]))))

(deftest angle-test
  (are [x y] (= x y)
       0.0 (angle [1 0] [1 0])
       m/HALF-PI (angle [1 0] [0 1])
       m/HALF-PI (angle [1 0 0 ] [0 1 0 ])
       m/HALF-PI (angle [1 0 0 0] [0 1 0 0])
       m/PI (angle [1 0] [-1 0])))
