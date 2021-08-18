
(ns binary-association-simulator.BinaryAssociator)

(def initialTransformCount 1000)

(defn randomUUID
  []
  (.toString (java.util.UUID/randomUUID)))

(defrecord Transform [data operation bitOperationIndex])

(defn randomTransform
  "Generates a random Transform record.
   Note that bit operation indexes can not be 0. WHY?"
  []
  (Transform. 1385473322 (rand-int 10) (rand-int 32)))

(defn vectorOfRandomTransforms
  []
  (loop [count initialTransformCount
         transforms []]
    (if (= 0 count)
      transforms
      (recur (dec count) (conj transforms (randomTransform))))))

(defrecord TransformationVector [transformations
                                 hammingDistanceTotal])

(defn randomTransformationVector
  []
  (TransformationVector. (vectorOfRandomTransforms)
                         (rand-int (Integer/MAX_VALUE))))

;; ------------------------------------------------------------------------------------------------------------------------
;; Running
;; ------------------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------------------------------------------------------------------------------

;; Operation Indicators
;; --------------------
;; See: https://cek.io/blog/2017/08/17/clojure-introduction-binary-bitwise/
;; AND : bit-and = 0
;; OR : bit-or = 1
;; XOR : bit-xor = 2
;; COMPLEMENT = bit-not = 3
;; AND WITH COMPLEMENT : bit-and-not = 4
;; SHIFT LEFT : bit-shift-left = 5
;; SHIFT RIGHT : bit-shift-right = 6
;; UNSIGNED SHIFT RIGHT : unsigned-bit-shift-right = 7
;; CLEAR BIT AT INDEX N : bit-clear = 8
;; FLIP BIT AT INDEX N : bit-flip = 9
;; SET BIT AT INDEX N : bit-set = 10
;; NO-OP : _ = 11 : LAST WINS
;; --------------------

(defn operate
  "Applies the operation passed to it with x and y as operands. n indicates the position in
   each integer to operate on for the index focused operations. In this case this operation
   is applied to y and its result is returned. In the case of a NO-OP x is returned. In the
   case of bit-not y is operated on."
  [operation x y n]
  (cond
    (= operation 0) (bit-and x y)
    (= operation 1) (bit-or x y)
    (= operation 2) (bit-xor x y)
    (= operation 3) (bit-not y)
    (= operation 4) (bit-and-not x y)
    (= operation 5) (unsigned-bit-shift-right x y)
    (= operation 6) (bit-clear x n)
    (= operation 7) (bit-flip x n)
    (= operation 8) (bit-set x n)
    (= operation 9) x)
    ;;(= operation 3) (bit-shift-left x y)
    ;;(= operation 4) (bit-shift-right x y)
    )

(defn propogateThroughTransformationVector
  "Propogates the given input data through the given Transformation Vector
   and returns the data."
  [transformationVector inputData]
  (loop [data inputData
         remainingTransformations (get transformationVector :transformations)]
    (if (empty? remainingTransformations)
      data
      (let [currentTransform (first remainingTransformations)
            operation (get currentTransform :operation)
            bitOperationIndex (get currentTransform :bitOperationIndex)
            embeddedData (get currentTransform :data)
            newData (operate operation data embeddedData bitOperationIndex)]
        (recur newData (rest remainingTransformations))))))
