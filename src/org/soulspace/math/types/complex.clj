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

(ns org.soulspace.math.types.complex
  "Contains the protocols and implementations for complex numbers."
  (:require [org.soulspace.math.complex :as mcp]))

;;;
;;; Protocols and Implementations for Complex Numbers
;;;

(declare complex)
(declare polar)

(defprotocol IComplex
  "Protocol for complex numbers in algebraic/cartesian form."
  (add [this c2] "Addition of complex 'this' number with the complex number 'c2'.")
  (substract [this c2] "Substraction of 'this' complex number with the complex number 'c2'.")
  (multiply [this c2] "Multiplication of 'this' complex number with the complex number 'c2'.")
  (divide [this c2] "Division of 'this' complex number with the complex number 'c2'.")
  (scalar-product [this c2] "Division of 'this' complex number with the real number 'x'.")
  (sqr [this] "Square of 'this' complex number.")
  (sqrt [this] "Square root of 'this' complex number.")
  (norm [this] "Absolute or norm of 'this' complex number.")
  (conjugate [this] "Conjugate of 'this' complex number.")
  (to-polar [this] "Polar form of 'this' complex number."))

(defprotocol IPolarComplex
  "Protocol for complex numbers in polar form."
  (sqrt-polar [p])
  (to-cartesian [p]))


(defrecord Complex
  [^double real ^double img]
  IComplex
  (add [this c2]
    (complex (mcp/add this c2)))
  (substract [this c2]
    (complex (mcp/substract this c2)))
  (multiply [this c2]
    (complex (mcp/multiply this c2)))
  (divide [this c2]
    (complex (mcp/divide this c2)))
  (scalar-product [this x]
    (complex (mcp/scalar-product this x)))
  (sqr [this]
    (multiply this this))
  (sqrt [this]
    (complex (mcp/sqrt this)))
  (norm [this]
    (mcp/norm this))
  (conjugate [this]
    (complex (mcp/conjugate this)))
  (to-polar [this]
    (polar (mcp/to-polar this))))

(defrecord PolarComplex
  [^double r ^double phi]
  IPolarComplex
  (sqrt-polar [this]
    (polar (mcp/sqrt-polar this)))
  (to-cartesian [p]
    (complex (mcp/to-cartesian p))))

;; constructors
(defn complex
  "Creates a complex number from real and imaginary parts."
  ([m]
   (map->Complex m))
  ([r i]
   (->Complex r i)))

(defn polar
  "Creates a complex number from polar coordinates."
  ([m]
   (map->PolarComplex m))
  ([r phi]
   (->PolarComplex r phi)))

;; constants
(def ZERO "The complex number zero." (complex 0 0))
(def ONE "The complex number one." (complex 1 0))
(def I "The complex number i" (complex 0 1))

