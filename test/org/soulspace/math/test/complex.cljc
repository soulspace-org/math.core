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
(ns org.soulspace.math.test.complex
  (:require [clojure.test :refer :all]
            [org.soulspace.math.complex :refer :all]))

(deftest zero-test
  (is (= ZERO {:real 0 :img 0})))

(deftest one-test
  (is (= ONE {:real 1 :img 0})))

(deftest i-test
  (is (= I {:real 0 :img 1})))

(deftest arithmetics-test
  (are [x y] (= x y)
       {:real 8 :img 7} (add {:real 3 :img 2} {:real 5 :img 5})
       {:real 2 :img 3} (substract {:real 5 :img 5} {:real 3 :img 2})
       {:real -43 :img 53} (multiply {:real 3 :img 5} {:real 4 :img 11})
       {:real 41/58 :img 1/58} (divide {:real 2 :img 5} {:real 3 :img 7})))
