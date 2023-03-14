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
            [org.soulspace.math.core :as m]
            [org.soulspace.math.geometry :refer :all]))

(deftest circle-circumference-test
  (is (= (circle-circumference 0) 0.0))
  (is (= (circle-circumference 1) m/DOUBLE-PI))
  (is (= (circle-circumference 2) (* 4 m/PI))))

(deftest circle-area-test
  (is (= (circle-area 0) 0.0))
  (is (= (circle-area 1) m/PI))
  (is (= (circle-area 2) (* 4 m/PI))))

