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
  (:require [org.soulspace.math.core :as m]))

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Geometric functions
;;;
(defn circle-circumference
  "Calculates the circumference of the circle with radius r."
  [r]
  (* 2 m/PI r))

(defn circle-area
  "Calculates the area of the circle with radius r."
  [r]
  (* m/PI (m/sqr r)))
