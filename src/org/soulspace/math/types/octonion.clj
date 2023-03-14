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

(ns org.soulspace.math.types.octonion
  (:require [org.soulspace.math.octonion :as mo]))

;;;
;;; Protocols and Implementations for Octonion Numbers
;;;

(declare octonion)

(defprotocol IOctonion
  "Protocol for octonions, hyper complex numbers of the 8th dimension."
  (add [this o2] "Returns the addition of 'this' octonion with the octonion o2.")
  (substract [this o2] "Returns the substraction of 'this' octonion with the octonion o2.")
  (multiply [this o2] "Returns the multiplication of 'this' octonion with the octonion o2.")
  (scalar-product [this o2] "Returns the scalar product of 'this' octonion with the number 'x'.")
  (conjugate [this] "Returns the conjugate o* of 'this' octonion.")
  (norm [this] "Returns the norm ||o|| of 'this' octonion.")
  (inverse [this] "Returns the inverse of 'this' octonion."))

(defrecord Octonion
  [r i j k l m n o]
  IOctonion
  (add [this o2]
    (octonion (mo/add this o2)))
  (substract [this o2]
    (octonion (mo/substract this o2)))
  (multiply [this o2]
    (octonion (mo/multiply this o2)))
  (conjugate [this]
    (octonion (mo/conjugate this)))
  (norm [this]
    (octonion (mo/norm this)))
  (inverse [this]
    (octonion (mo/inverse this))))

(defn octonion
  "Creates a new octonion from octonian map 'om' or the real numbers
   'r', 'i', 'j', 'k', 'l', 'm', 'n' and 'o'."
  ([om]
    (map->Octonion om))
  ([r i j k l m n o]
    (->Octonion r i j k l m n o)))
