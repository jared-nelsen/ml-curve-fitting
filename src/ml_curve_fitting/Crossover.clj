
(ns ml-curve-fitting.Crossover
  (:require [ml-curve-fitting.BezierCurve :as bCurve]))

(defn takeFitterParent
  "Returns the fitter Bezier Curve of the given two."
  [parentA parentB]
  (let [aFitness (get parentA :fitness)
        bFitness (get parentB :fitness)]
    (if (< aFitness bFitness)
      parentA
      parentB)))

(defn takeVectorByLength
  "Given the operator < or >= return the vector that meets the check."
  [operator vectorA vectorB]
  (let [vectorALength (count vectorA)
        vectorBLength (count vectorB)]
    (if (operator vectorALength vectorBLength)
      vectorA
      vectorB)))

(defn takeRemainingItems
  "Flips a coin. If it succeeds then all of vector b's elements will be added into
   the tail of a."
  [a b]
  (if (< (rand) 0.5)
    (into a b)
    a))

(defn crossoverControlPoints
  "Crosses over the X and Y data in the given Control Points. Flip a coin on which
   parent to take X and Y from. This may need to be revisited."
  [pointA pointB]
  (if (< (rand) 0.5)
    (assoc pointA :x (get pointB :x))
    (assoc pointA :y (get pointB :y))))

(defn crossoverControlPointVectors
  "Crosses over the given Control Point vector. If vectors are of differing sizes
   then they are crossed over in order up until the point that the shorter vector
   is spent. Then if there are remaining items in the longer vector there is a
   50/50 chance they will be taken along with the new vector."
  [pointVectorA pointVectorB]
  (loop [shorterVector (takeVectorByLength < pointVectorA pointVectorB)
         longerVector (takeVectorByLength >= pointVectorA pointVectorB)
         newVector []]
    (if (empty? shorterVector)
      (takeRemainingItems newVector longerVector)
      (let [pointA (first shorterVector)
            pointB (first longerVector)
            resultantPoint (crossoverControlPoints pointA pointB)]
        (recur (rest shorterVector)
               (rest longerVector)
               (conj newVector resultantPoint))))))

(defn crossoverGivenBezierCurves
  "Crosses over the data between the given Bezier Curves. If the Crossover
   attempt fails then the fitter parent is taken."
  ([crossoverRate curveA curveB]
   (if (< (rand) crossoverRate)
     (crossoverGivenBezierCurves curveA curveB)
     (takeFitterParent curveA curveB)))
  ([curveA curveB]
   (let [controlPointVectorA (get curveA :controlPointVector)
         controlPointVectorB (get curveB :controlPointVector)
         crossedOverVectors (crossoverControlPointVectors controlPointVectorA
                                                          controlPointVectorB)
         crossedOverVectors (bCurve/resetXIndecesOfControlPointVector crossedOverVectors)]
     (assoc curveA :controlPointVector crossedOverVectors))))

(defn crossover
  "Crosses over members of the selected population. Members come in in pairs
   in the input population and for each member N it will be crossed over
   with member N + 1 into one new member. The pattern continues with the
   next member at N + 2."
  [algorithmContext]
  (loop [population (get algorithmContext :population)
         newPopulation []]
    (if (empty? population)
      (assoc algorithmContext :population newPopulation)
      (let [member1 (first population)
            member2 (first (rest population))
            crossoverRate (get algorithmContext :crossoverRate)
            newMember (crossoverGivenBezierCurves crossoverRate
                                                  member1
                                                  member2)]
        (recur (rest (rest population))
               (conj newPopulation newMember))))))
