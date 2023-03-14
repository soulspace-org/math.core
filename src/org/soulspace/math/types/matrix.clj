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

(ns org.soulspace.math.types.matrix
  (:require [org.soulspace.math.matrix :as mm]))

;;;
;;; Protocols and Implementations for Matrices
;;;

(defprotocol IMatrix
  "Protocol for matrices."
  (element [this i j] "Returns the element of 'this' matrix at row 'i' and column 'j'.")
  (row-vector [this i] "Returns the row vector of 'this' matrix at row 'i'.")
  (column-vector [this j] "Returns the column vector of 'this' matrix at column 'j'.")
  (upper-triangular [this] "Returns the upper triangular of 'this' matrix.")
  (lower-triangular [this] "Returns the upper triangular of 'this' matrix.")
  (scalar-sum [this x] "Calculates the scalar sum of 'this' matrix with the scalar 'x'.")
  (scalar-product [this x] "Calculates the scalar product of 'this' matrix with the scalar 'x'.")
  (matrix-sum [this m2] "Calculates the sum of 'this' matrix with the matrix 'm2'.")
  (matrix-product [this m2] "Calculates the matrix product of 'this' matrix with matrix 'm2'.")
  (transpose [this] "Returns the transposed matrix of this matrix.")
  (solve [this v] "Calculates the solution of the linear equations of 'this' matrix with the vector 'v'."))


(defrecord VecMatrix
  [elements]
  IMatrix
  (element [this i j]
    (mm/element this i j))
  (row-vector [this i]
    (mm/row-vector this i))
  (column-vector [this j]
    (mm/column-vector this j))
  (upper-triangular [this]
    (mm/upper-triangular this))
  (lower-triangular [this]
    (mm/lower-triangular this))
  (scalar-sum [this x]
    (mm/scalar-sum this x))
  (scalar-product [this x]
    (mm/scalar-product this x))
  (matrix-sum [this m2]
    (mm/matrix-sum this m2))
  (matrix-product [this m2]
    (mm/matrix-product this m2))
  (transpose [this]
    (mm/transpose this))
  (solve [this v]))
