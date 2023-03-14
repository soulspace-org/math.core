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
(ns org.soulspace.math.types.test.complex
  (:require [clojure.test :refer :all]
            [org.soulspace.math.types.complex :as c]))
  ;(:import [org.soulspace.math.complex DoublePolarComplexImpl DoubleComplexImpl])


(deftest complex-test)

(deftest zero-test
  (is (= c/ZERO (c/complex 0 0))))

(deftest one-test
  (is (= c/ONE (c/complex 1 0))))

(deftest i-test
  (is (= c/I (c/complex 0 1))))

(deftest add-test
  (is (= (c/add c/ZERO c/ZERO) c/ZERO))
  (is (= (c/add c/ONE c/ZERO) c/ONE))
  (is (= (c/add c/ZERO c/ONE) c/ONE))
  (is (= (c/add c/I c/ZERO) c/I))
  (is (= (c/add c/ZERO c/I) c/I))
  (is (= (c/add c/ONE c/ONE) (c/complex 2 0)))
  (is (= (c/add c/I c/I) (c/complex 0 2)))
  (is (= (c/add c/ONE c/I) (c/complex 1 1)))
  (is (= (c/add c/I c/ONE) (c/complex 1 1)))
  (is (= (reduce c/add c/ZERO [c/ZERO c/ZERO]) c/ZERO))
  (is (= (reduce c/add c/ONE [c/ONE c/ONE]) (c/complex 3 0))))

(deftest mult-test
  (is (= (c/multiply c/ONE c/ONE) c/ONE))
  (is (= (c/multiply c/ZERO c/ONE) c/ZERO))
  (is (= (c/multiply c/ONE c/ZERO) c/ZERO))
  (is (= (c/multiply c/ONE c/I) c/I))
  (is (= (c/multiply c/I c/ONE) c/I)))

(deftest sub-test
  (is (= (c/substract c/ZERO c/ZERO) c/ZERO)))

(deftest div-test
  (is (= (c/divide c/ONE c/ONE) c/ONE)))

(deftest conjugate-test
  (is (= (c/conjugate (c/complex 1 3)) (c/complex 1 -3)))
  (is (= (c/conjugate (c/complex 1 -3)) (c/complex 1 3))))


;(run-tests)
