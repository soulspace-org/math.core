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
(ns org.soulspace.math.quaternion
  (:require [org.soulspace.math.core :as m]))

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Quarternions, hyper complex numbers of 4th dimension
;;;
;;; (see Quarternion on wikipedia.org)
;;;

;;
;; Constants
;;

(def zero   "Zero element."           {:r 0 :i 0 :j 0 :k 0})
(def one    "Identity element."       {:r 1 :i 0 :j 0 :k 0})
(def r-unit "Unit of the r comonent." {:r 1 :i 0 :j 0 :k 0}) ; same as the identity element
(def i-unit "Unit of the i comonent." {:r 0 :i 1 :j 0 :k 0})
(def j-unit "Unit of the j comonent." {:r 0 :i 0 :j 1 :k 0})
(def k-unit "Unit of the k comonent." {:r 0 :i 0 :j 0 :k 1})

;;
;; Functions
;;

(defn add
  "Calculates the addition of the quaternion numbers 'q1' and 'q2'."
  [q1 q2]
  {:r (+ (:r q1) (:r q2))
   :i (+ (:i q1) (:i q2))
   :j (+ (:j q1) (:j q2))
   :k (+ (:k q1) (:k q2))})

(defn substract
  "Calculates the substraction of the quaternion numbers 'q1' and 'q2'."
  [q1 q2]
  {:r (- (:r q1) (:r q2))
   :i (- (:i q1) (:i q2))
   :j (- (:j q1) (:j q2))
   :k (- (:k q1) (:k q2))})

(defn multiply
  "Calculates the multiplication or hamilton product of the quaternion numbers 'q1' and 'q2'."
  [q1 q2]
  {:r (- (* (:r q1) (:a q2)) (* (:i q1) (:i q2)) (* (:j q1) (:j q2)) (* (:k q1) (:k q2)))
   :i (+ (* (:r q1) (:i q2)) (* (:i q1) (:r q2)) (* (:j q1) (:k q2)) (* -1 (:k q1) (:j q2)))
   :j (+ (* (:r q1) (:j q2)) (* -1 (:i q1) (:k q2)) (* (:j q1) (:r q2)) (* (:k q1) (:i q2)))
   :k (+ (* (:r q1) (:k q2)) (* (:i q1) (:j q2)) (* -1 (:j q1) (:i q2)) (* (:k q1) (:r q2)))})

(defn scalar-product
  "Calculates the scalar product of the quaternion 'q' with the real number 'x'"
  [q x]
  {:r (* x (:r q))
   :i (* x (:i q))
   :j (* x (:j q))
   :k (* x (:k q))})

(def hamilton-product
  "Calculates the hamilton product of the quaternion numbers 'q1' and 'q2'."
  multiply)

(defn scalar-product
  "Calculates the hamilton product of the quaternion numbers 'q1' and 'q2'."
  [q1 q2]
  (+ (* (:r q1) (:r q2)) (* (:i q1) (:i q2)) (* (:j q1) (:j q2)) (* (:k q1) (:k q2))))

(defn conjugate
  "Calculates the conjugate q* of the quaternion number 'q'."
  [q]
  {:r (:r q)
   :i (* -1 (:i q))
   :j (* -1 (:j q))
   :k (* -1 (:k q))})

(defn norm
  "Calculates the norm ||q|| of the quaternion number 'q'."
  [q]
  (m/sqrt (+ (m/sqr (:r q)) (m/sqr (:i q)) (m/sqr (:j q)) (m/sqr (:k q)))))

(defn inverse
  "Calculates the inverse of the quaternion number 'q', if q is not zero.
  If q is zero, an exeption is thrown."
  [q]
  (scalar-product (conjugate q) (/ 1 (norm q))))

