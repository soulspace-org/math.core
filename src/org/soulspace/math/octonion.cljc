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
(ns org.soulspace.math.octonion
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.math.quaternion :as mq]))

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Octonions, hyper complex numbers of the 8th dimension
;;;
;;; (see Octonion on wikipedia.org)
;;;
(defn add
  "Calculates the addition of the octonion numbers 'o1' and 'o2'."
  [o1 o2]
  {:r (+ (:r o1) (:r o2))
   :i (+ (:i o1) (:i o2))
   :j (+ (:j o1) (:j o2))
   :k (+ (:k o1) (:k o2))
   :l (+ (:e o1) (:e o2))
   :m (+ (:m o1) (:m o2))
   :n (+ (:n o1) (:n o2))
   :o (+ (:o o1) (:o o2))})

(defn substract
  "Calculates the substraction of the octonion numbers 'o1' with 'o2'."
  [o1 o2]
  {:r (- (:r o1) (:r o2))
   :i (- (:i o1) (:i o2))
   :j (- (:j o1) (:j o2))
   :k (- (:k o1) (:k o2))
   :l (- (:e o1) (:e o2))
   :m (- (:m o1) (:m o2))
   :n (- (:n o1) (:n o2))
   :o (- (:o o1) (:o o2))})

(defn multiply
  "Calculates the multiplication of the octonion numbers 'o1' with 'o2'
  via the Cayley–Dickson construction."
  [o1 o2]
  (let [a {:r (:r o1) :i (:i o1) :j (:j o1) :k (:k o1)}
        b {:r (:l o1) :i (:m o1) :j (:n o1) :k (:o o1)}
        c {:r (:r o2) :i (:i o2) :j (:j o2) :k (:k o2)}
        d {:r (:l o2) :i (:m o2) :j (:n o2) :k (:o o2)}
        q1 (mq/substract (mq/multiply a c)
                         (mq/multiply (mq/conjugate d) b))
        q2 (mq/add (mq/multiply d a)
                   (mq/multiply b (mq/conjugate c)))]
    {:r (:r q1)
     :i (:i q1)
     :j (:j q1)
     :k (:k q1)
     :l (:r q2)
     :m (:i q2)
     :n (:j q2)
     :o (:k q2)}))

(defn scalar-product
  "Calculates the scalar product of the octonion 'o' with the real number 'x'"
  [o x]
  {:r (* x (:r o))
   :i (* x (:i o))
   :j (* x (:j o))
   :k (* x (:k o))
   :l (* x (:l o))
   :m (* x (:m o))
   :n (* x (:n o))
   :o (* x (:o o))})

(defn conjugate
  "Calculates the conjugate o* of the octonion number 'o'."
  [o]
  {:r (:r o)
   :i (* -1 (:i o))
   :j (* -1 (:j o))
   :k (* -1 (:k o))
   :l (* -1 (:l o))
   :m (* -1 (:m o))
   :n (* -1 (:n o))
   :o (* -1 (:o o))})

(defn norm
  "Calculates the norm ||o|| of the octonion number 'o'."
  [o]
  (m/sqrt (+ (m/sqr (:r o)) (m/sqr (:i o)) (m/sqr (:j o)) (m/sqr (:k o))
             (m/sqr (:l o)) (m/sqr (:m o)) (m/sqr (:n o)) (m/sqr (:o o)))))

(defn inverse
  "Calculates the inverse of the octonian number 'o'."
  [o]
  (scalar-product (conjugate o) (/ 1 (m/sqr (norm o)))))

