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
(ns org.soulspace.math.matrix)

#?(:clj
   (set! *warn-on-reflection* true))

;;;
;;; Matrix functions
;;;

(defn rows
  "Returns the number of rows of the matrix m."
  [m]
  (count m))

(defn columns
  "Returns the number of columns of the matrix m."
  [m]
  (count (m 0)))

(defn shape
  "Returns the shape of the matrix as a vector of the number of rows and columns."
  [m]
  [(count m) (count (m 0))])

(defn element
  "Returns the element of the matrix m at row i and column j."
  [m i j]
  (nth (nth m i) j))

(defn row-vector
  "Returns the row vector of the matrix m at row i."
  [m i]
  (nth m i))

(defn column-vector
  "Returns the column vector of the matrix m at column j."
  [m j]
  (mapv #(nth % j) m))

(defn transpose
  "Returns the transposed matrix of the matrix m."
  [m]
  (mapv #(column-vector m %) (range (columns m))))

(def column-vectors
  "Returns the column vectors of the matrix m."
  transpose)

(defn diagonal
  "Returns 1 if i = j, otherwise 0."
  [i j]
  (if (= i j)
    1
    0))

(defn build-matrix
  "Builds a new matrix of shape rows x cols with the value m[i, j] is f(i, j)."
  [rows cols f]
  (vec (for [i (range rows)]
         (vec (for [j (range cols)]
                (f i j))))))

(defn identity-matrix
  "Builds an identity matrix of shape n x n."
  [n]
  (build-matrix n n diagonal))

(defn scalar-add
  "Adds the scalar s to the matrix m."
  [s m]
  (mapv (partial mapv (partial + s)) m))

(defn scalar-product
  "Multiplies the scalar s to the matrix m."
  [s m]
  (mapv (partial mapv (partial * s)) m))

(defn matrix-add
  "Adds the matrices m and n."
  [m n]
  (if (= (shape m) (shape n))
    (mapv (partial mapv +) m n)
    (throw (ex-info "The matrices are not of the same shape." {:shape-m (shape m)
                                                               :shape-n (shape n)}))))

(defn matrix-substract
  "Substracts the matrices m and n."
  [m n]
  (if (= (shape m) (shape n))
    (mapv (partial mapv -) m n)
    (throw (ex-info "The matrices are not of the same shape." {:shape-m (shape m)
                                                               :shape-n (shape n)}))))

(defn matrix-sum
  "Adds the matrices."
  [& ms]
  (reduce matrix-add ms))

(defn matrix-product
  [m n]
  (if (= (columns m) (rows n))
    (vec (for [i (range (rows m))]
           (vec (for [k (range (columns n))]
                  (apply + (for [j (range (columns m))]
                             (* (element m i j) (element n j k))))))))
    (throw (ex-info "The matrices are not compatible." {:colums-m (columns m)
                                                        :rows-n (rows n)}))))


; TODO check if this should be compliant to an L-R Zerlegung
(defn upper-triangular
  "Returns the upper triangular matrix of the matrix m."
  [m]
  (vec (for [i (range (rows m))]
         (vec (for [j (range (columns m))]
                (if (<= i j)
                  (element m i j)
                  0))))))

; TODO check if this should be compliant to an L-R Zerlegung
(defn lower-triangular
  "Returns the lower triangular matrix of the matrix m."
  [m]
  (vec (for [i (range (rows m))]
         (vec (for [j (range (columns m))]
                (if (>= i j)
                  (element m i j)
                  0))))))

; TODO
(defn solve
  [m v])

