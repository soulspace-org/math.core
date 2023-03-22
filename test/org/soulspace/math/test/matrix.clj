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

(ns org.soulspace.math.test.matrix
  (:require [clojure.test :refer :all]
            [org.soulspace.math.matrix :as mm]))

(deftest shape-test
   (testing "Matrix shape"
     (are [x y] (= x y)
       [1 1] (mm/shape [[0]])
       [2 1] (mm/shape [[0] [1]])
       [1 2] (mm/shape [[0 1]])
       [2 3] (mm/shape [[1 2 3]
                     [4 5 6]]))))

(deftest identity-matrix-test
  (testing "Identity matrix test"
    (are [x y] (= x y)
      [[1]] (mm/identity-matrix 1)
      [[1 0]
       [0 1]] (mm/identity-matrix 2)
      [[1 0 0]
       [0 1 0]
       [0 0 1]] (mm/identity-matrix 3))))

(deftest scalar-sum-test
  (are [x y] (= x y)
    [[0]] (mm/scalar-add 0 [[0]])
    [[1]] (mm/scalar-add 1 [[0]])
    [[1]] (mm/scalar-add 0 [[1]])
    [[2]] (mm/scalar-add 1 [[1]])
    [[0 1]
     [1 0]] (mm/scalar-add 0 [[0 1]
                              [1 0]])
    [[1 2]
     [2 1]] (mm/scalar-add 1 [[0 1]
                              [1 0]])))

(deftest scalar-product-test
  (are [x y] (= x y)
    [[0]] (mm/scalar-product 0 [[0]])
    [[0]] (mm/scalar-product 1 [[0]])
    [[0]] (mm/scalar-product 0 [[1]])
    [[1]] (mm/scalar-product 1 [[1]])
    [[2]] (mm/scalar-product 2 [[1]])
    [[2]] (mm/scalar-product 1 [[2]])
    [[0 0]
     [0 0]] (mm/scalar-product 0 [[0 1]
                                  [1 0]])
    [[0 1]
     [1 0]] (mm/scalar-product 1 [[0 1]
                                  [1 0]])
    [[0 2]
     [2 0]] (mm/scalar-product 2 [[0 1]
                                  [1 0]])))

