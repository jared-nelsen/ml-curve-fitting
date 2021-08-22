
(ns ml_curve_fitting.Mutate
  (:require [ml_curve_fitting.BezierCurve :as bCurve]))

(def valueBumpLow -0.001)
(def valueBumpHigh 0.001)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

;;-----------------------------------------------------------------------------------
;; Add and Remove
;;-----------------------------------------------------------------------------------

(defn addItemToVectorAtIndex
  "Adds the given item to the given vector at the given index."
  [vector item index]
  (let [size (count vector)
        precedent (subvec vector 0 index)
        precedent (conj precedent item)
        antecedent (subvec vector index size)]
    (into precedent antecedent)))

(defn removeItemFromVectorAtIndex
  "Removes the item that resides at the given index from the
   given vector."
  [vector index]
  (vec (concat (subvec vector 0 index) (subvec vector (inc index)))))

(defn addRandomControlPointToBezierCurve
  "Adds a random Control Point to a random index in the given Bezier Curve."
  ([bezierCurve mutationRate]
   (if (< (rand) mutationRate)
     (addRandomControlPointToBezierCurve bezierCurve)
     bezierCurve))
  ([bezierCurve]
   (let [controlPointVector (get bezierCurve :controlPointVector)
         ;; Give the new Control Point an invalid X index. X indeces will be rectified in the next few steps
         newRandomControlPoint (bCurve/->ControlPoint -1 (bCurve/randomDoubleInARange bCurve/defaultYMin
                                                                                      bCurve/defaultYMax))
         addIndex (rand-int (count controlPointVector))
         updatedControlPointVector (addItemToVectorAtIndex controlPointVector
                                                           newRandomControlPoint
                                                           addIndex)
         updatedControlPointVector (bCurve/resetXIndecesOfControlPointVector updatedControlPointVector)]
     (assoc bezierCurve :controlPointVector updatedControlPointVector))))

(defn randomlyRemoveControlPointFromBezierCurve
  "Randomly removes a Control Point from the given Bezier Curve."
  ([bezierCurve mutationRate]
   (if (< (rand) mutationRate)
     (randomlyRemoveControlPointFromBezierCurve bezierCurve)
     bezierCurve))
  ([bezierCurve]
   (let [controlPointVector (get bezierCurve :controlPointVector)
         removeIndex (rand-int (count controlPointVector))
         updatedControlPointVector (removeItemFromVectorAtIndex controlPointVector
                                                                removeIndex)
         updatedControlPointVector (bCurve/resetXIndecesOfControlPointVector updatedControlPointVector)]
     (assoc bezierCurve :controlPointVector updatedControlPointVector))))

;;----------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------
;; Control Points
;;-----------------------------------------------------------------------------------

(defn mutateControlPointValue
  "Mutates the given value in the bumping range according to the mutation rate."
  [value mutationRate]
  (if (<= (rand) mutationRate)
    (+ value (randomDoubleInARange valueBumpLow valueBumpHigh))
    value))

(defn mutateControlPoint
  "Mutates the X and Y values of the given Control Point."
  ([controlPoint mutationRate]
   (if (< (rand) mutationRate)
     (mutateControlPoint controlPoint)
     controlPoint))
  ([controlPoint]
   (let [newX (mutateControlPointValue (get controlPoint :x))
         newY (mutateControlPointValue (get controlPoint :y))]
     (assoc controlPoint :x newX :y newY))))

(defn mutateControlPointsInBezierCurve
  "Mutates the Control Points in the given Bezier Curve."
  [bezierCurve mutationRate]
  (loop [mutatedControlPoints []
         controlPoints (get :controlPointVector bezierCurve)]
    (if (empty? controlPoints)
      mutatedControlPoints
      (recur (conj mutatedControlPoints (mutateControlPoint (first controlPoints) mutationRate))
             (rest controlPoints)))))
;;----------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------
;; Main Mutation Function
;;-----------------------------------------------------------------------------------

(defn mutateBezierCurve
  "Mutates the given Bezier Curve."
  [bezierCurve mutationRate]
  (let [mutatedBCurve (mutateControlPointsInBezierCurve bezierCurve mutationRate)
        mutatedBCurve (randomlyRemoveControlPointFromBezierCurve bezierCurve mutationRate)
        mutatedBCurve (addRandomControlPointToBezierCurve bezierCurve mutationRate)]
    mutatedBCurve))

(defn mutate
  "Takes in the algorithm context and mutates all population
   members according to the mutation rate."
  [context]
  (let [population (get context :population)
        mutationRate (get context :mutationRate)]
    (loop [pop population
           newPop []]
      (if (empty? pop)
        (assoc context :population newPop)
        (let [popMember (first pop)
              mutatedPopMember (mutateBezierCurve popMember mutationRate)]
          (recur (rest pop) (conj newPop mutatedPopMember)))))))
