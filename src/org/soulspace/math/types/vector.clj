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

(ns org.soulspace.math.types.vector
  (:refer-clojure :exclude [vector])
  (:require [org.soulspace.math.vector :as mv]))

;;;
;;; Protocols and Implementations for Vectors
;;;

(set! *warn-on-reflection* true)

(declare vector)

(defprotocol IVector
  "Protocol for vectors."
  (scalar-add [this x] "Calculates the scalar addition of 'this' vector with the scalar 'x'.")
  (scalar-product [this x] "Calculates the scalar product of 'this' octonion with the scalar 'x'.")
  (add [this v2] "Adds the vector 'v2' to 'this' vector.")
  (substract [this v2] "Substracts the vector 'v2' from 'this' vector.")
  (dot-product [this v2] "Calculates the dot product of 'this' vector with vector'v2'.")
  (norm [this] "Calculates the norm or magnitude of 'this' vector.")
  (distance [this m2] "Calculates the distance of 'this' vector and the vector 'm2'."))

(defrecord Vector
  [elements]
  IVector
  (scalar-add [this x]
    (mv/scalar-add (:elements this) x))
  (scalar-product [this x]
    (mv/scalar-product (:elements this) x))
  (add [this v2]
    (vector (mv/add (:elements this) (:elements v2))))
  (substract [this v2]
    (vector (mv/substract (:elements this) (:elements v2))))
  (dot-product [this v2]
    (mv/dot-product (:elements this) (:elements v2)))
  (norm [this]
    (mv/norm this))
  (distance [this m2]
    (mv/distance this m2)))

(defn vector
  "Creates a new vector instance from the elements 'e'."
  [e]
  (->Vector e))