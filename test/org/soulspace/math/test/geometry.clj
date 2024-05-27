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

(ns org.soulspace.math.test.geometry
  (:require [clojure.test :refer :all]
            [clojure.math :as m]
            [org.soulspace.math.core :as mc]
            [org.soulspace.math.geometry :as mg]))

(deftest circle-circumference-test
  (testing "Testing circle-circumference"
    (is (= 0.0          (mg/circle-circumference 0)))
    (is (= m/PI         (mg/circle-circumference 0.5)))
    (is (= mc/DOUBLE-PI (mg/circle-circumference 1)))
    (is (= (* 4 m/PI)   (mg/circle-circumference 2)))
    (is (= (* 8 m/PI)   (mg/circle-circumference 4)))))

(deftest circle-area-test
  (testing "Testing circle-area"
    (is (= 0.0            (mg/circle-area 0)))
    (is (= (* m/PI 0.25)  (mg/circle-area 0.5)))
    (is (= m/PI           (mg/circle-area 1)))
    (is (= (* 4 m/PI)     (mg/circle-area 2)))
    (is (= (* 16 m/PI)    (mg/circle-area 4)))))

(deftest diagonal-test
  (testing "Testing diagonal"
    (is (= 0.0 (mg/rectangle-diagonal 0)))
    (is (= 5.0 (mg/rectangle-diagonal 3 4)))))
