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

(ns org.soulspace.math.geometry
  (:require [clojure.math :as m]
            [org.soulspace.math.core :as mc]))

;;;
;;; Geometric functions
;;;
(defn circle-circumference
  "Calculates the circumference of the circle with radius `r`."
  ^double [r]
  (* 2 m/PI r))

(defn circle-area
  "Calculates the area of the circle with radius `r`."
  ^double [r]
  (* m/PI (mc/sqr r)))

(defn rectangle-circumference
  "Calculates the circumference of a square with sides `a` or a  rectangle of sides `a` and `b`."
  ([a]
   (* 4 a))
  ([a b]
   (+ (* 2 a) (* 2 b))))

(defn rectangle-area
  "Calculates the area of a square with sides `a` or a  rectangle of sides `a` and `b`."
  ([a]
   (* a a))
  ([a b]
   (* a b)))

(defn rectangle-diagonal
  "Calculates the diagonal of a square with sides `a` or a rectangle of sides `a` and `b`."
  (^double [a]
   (m/sqrt (* 2 a a)))
  (^double [a b]
   (m/sqrt (+ (* a a) (* b b)))))

