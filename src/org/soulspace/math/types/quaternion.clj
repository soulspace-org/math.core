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

(ns org.soulspace.math.types.quaternion
  (:require [org.soulspace.math.core :as m]
            [org.soulspace.math.quarternion :as mq]))

;;;
;;; Protocols and Implementations for Quaternion Numbers
;;;

(declare quaternion)

(defprotocol IQuaternion
  "Protocol for Quaternions, also known as Hamilton numbers, hyper complex numbers of 4th dimension."
  (add [this q2] "Returns the addition of 'this' quaternion with the quaternion 'q2'.")
  (substract [this q2] "Returns the substraction of 'this' quaternion with the quaternion 'q2'.")
  (multiply [this q2] "Returns the multiplication of 'this' quaternion with the quaternion 'q2'.")
  (scalar-product [this x] "Calculates the scalar product of 'this' quaternion with the number 'x'.")
  (conjugate [this] "Returns the conjugate q* of 'this' quaternion.")
  (norm [this] "Returns the norm of 'this' quaternion."))

(defrecord Quaternion
  [r i j k]
  IQuaternion
  (add [this q2]
    (quaternion (mq/add this q2)))
  (substract [this q2]
    (quaternion (mq/add this q2)))
  (multiply [this q2]
    (quaternion (mq/mult this q2)))
  (scalar-product [this x]
    (quaternion (mq/scalar-product this x)))
  (conjugate [this]
    (quaternion (mq/conjugate this)))
  (norm [this]
    (mq/norm this)))

(defn quaternion
  "Creates a new quarternion from the quaternion map 'q' or the real numbers 'r', 'i', 'j' and 'k'."
  ([q]
    (map->Quaternion q))
  ([r i j k]
    (->Quaternion r i j k)))
