(ns binary-association-simulator.Data)
;; This file constitutes the area of functionality around generating simulation data.
;;
;; Transformation Vector Data Format
;; ---------------------------------
;; Example Input Frame:   [0  1  2   3   4  5]
;; Example Output Frame:  [20 34 138 554 78 990]
;;
;; Binary Associator Data Format
;; -----------------------------
;; Example Data Frame :   [[TV Data] [TV Data] [TV Data]]

(defn generateTransformationVectorInputFrame
  "An input frame is a vector of length N composed of increasing integers
   starting at index 0."
  [N]
  (loop [n 0
         frame []]
    (if (= n N)
      frame
      (recur (inc n) (conj frame n)))))

(defn generateTransformationVectorOutputFrame
  "An output frame is a vector of length N composed of arbitrary random
   integers."
  [N]
  (vec (take N (repeatedly #(rand-int Integer/MAX_VALUE)))))

(defn generateTransformationVectorSimulationData
  [inputOutputPairCount]
  {:inputOutputPairCount inputOutputPairCount
   :inputFrame (generateTransformationVectorInputFrame inputOutputPairCount)
   :outputFrame (generateTransformationVectorOutputFrame inputOutputPairCount)})
