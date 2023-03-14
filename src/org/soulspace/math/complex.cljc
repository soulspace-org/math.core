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

(ns org.soulspace.math.complex
  "Functions for complex numbers in cartesian or polar representation.

  Complex numbers are represented as maps with the keys ':real' and ':img'
  in cartesian representation and ':r' and ':phi' in polar representation."
  (:require [org.soulspace.math.core :as m]))

;;
;; Complex numbers
;;
#?(:clj
   (set! *warn-on-reflection* true))

;;
;; Complex constants (in cartesian representation)
;;

(def ZERO "The complex number zero in cartesian representation." {:real 0 :img 0})
(def ONE  "The complex number one in cartesian representation."  {:real 1 :img 0})
(def I    "The complex number i in cartesian representation"     {:real 0 :img 1})

;;
;; Conversion of representations
;;

(defn to-polar
  "Retuns the polar representation of the complex number 'c' given in cartesian representation."
  [c]
  (cond
    (and (== (:real c) 0)
         (== (:img c) 0)) {:r   0
                           :phi 0}
    (== (:real c) 0)      {:r   (:img c)
                           :phi (if (< (:img c) 0) (/ m/PI 2) (/ (* 3 m/PI) 2))}
    (< (:real c) 0)       {:r   (m/sqrt (+ (m/sqr (:real c)) (m/sqr (:img c))))
                           :phi (+ m/PI (m/atan (/ (:img c) (:real c))))}
    :default              {:r   (m/sqrt (+ (m/sqr (:real c)) (m/sqr (:img c))))
                           :phi (m/atan (/ (:img c) (:real c)))}))

(defn to-cartesian
  "Retuns the cartesian representation of the complex number 'p' given in polar representation."
  [p]
  {:real (* (:r p) (m/cos (:phi p)))
   :img  (* (:r p) (m/sin (:phi p)))})

;;
;; Functions for polar representation
;;

(defn mult-polar
  "Multiplies the complex numbers given in polar representation."
  [p1 p2]
  {:r (* (:r p1) (:r p2))
   :phi (+ (:phi p1) (:phi p2))})

(defn div-polar
  "Divides the complex numbers given in polar representation."
  [p1 p2]
  {:r (/ (:r p1) (:r p2))
   :phi (- (:phi p1) (:phi p2))})

(defn sqrt-polar
  "Calculates the principal square root of the complex number 'p' given in polar representation."
  [p]
  (let [sqrt-r (m/sqrt (:r p))]
    {:r   (* sqrt-r (m/cos (/ (:phi p) 2)))
     :phi (* sqrt-r (m/sin (/ (:phi p) 2)))}))

;;
;; Functions for cartesian representation
;;

(defn add
  "Adds two complex numbers 'c1' and 'c2' given in cartesian representation."
  [c1 c2]
  {:real (+ (:real c1) (:real c2))
   :img  (+ (:img c1) (:img c2))})

(defn substract
  "Substracts two complex numbers 'c1' and 'c2' given in cartesian representation'."
  [c1 c2]
  {:real (- (:real c1) (:real c2))
   :img  (- (:img c1) (:img c2))})

(defn multiply
  "Multiplies two complex numbers 'c1' and 'c2' given in cartesian representation."
  [c1 c2]
  {:real (- (* (:real c1) (:real c2)) (* (:img c1) (:img c2)))
   :img  (+ (* (:real c1) (:img c2)) (* (:img c1) (:real c2)))})

(defn divide
  "Multiplies two complex numbers 'c1' and 'c2' given in cartesian representation."
  [c1 c2]
  {:real (/ (+ (* (:real c1) (:real c2)) (* (:img c1) (:img c2)))
            (+ (m/sqr (:real c2)) (m/sqr (:img c2))))
   :img  (/ (- (* (:img c1) (:real c2)) (* (:real c1) (:img c2)))
            (+ (m/sqr (:real c2)) (m/sqr (:img c2))))})

(defn scalar-product
  "Calculates the scalar product of the complex number 'c' with the real number 'x'"
  [c x]
  {:real (* x (:real c))
   :img  (* x (:img c))})

(defn sqr
  "Calculates the square of the complex number 'c' given in cartesian repmresentation."
  [c]
  (multiply c c))

(defn sqrt
  "Calculates the principal square root of the complex number 'c' given in cartesian representation."
  [c]
  (to-cartesian (sqrt-polar (to-polar c))))

(defn norm
  "Calculates the norm or absolute value of the complex number 'c' given in cartesian representation."
  [c]
  (m/sqrt (+ (m/sqr (:real c)) (m/sqr (:img c)))))

(defn conjugate
  "Calculates the conjugate of the complex number 'c' given in cartesian representation."
  [c]
  {:real (:real c)
   :img  (* -1 (:img c))})

(defn inverse
  "Calculates the conjugate of the complex number 'c' given in cartesian representation.
  If c is zero, an exception is thrown."
  [c]
  (scalar-product (conjugate c) (/ 1 (norm c))))
