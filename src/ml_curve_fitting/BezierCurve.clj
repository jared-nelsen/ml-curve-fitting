
(ns ml-curve-fitting.BezierCurve)

(def defaultYMin -150.0)
(def defaultYMax 150.0)
(def defaultXMin -150.0)
(def defaultXMax 150.0)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

(defn resetXIndecesOfControlPointVector
  "Sets the X indeces in each Control Point in the given Control Point vector to its own position
   in the given vector."
  [controlPointVector]
  (loop [currentIndex 0
         updatedControlPointVector []
         controlPointVector controlPointVector]
    (if (empty? controlPointVector)
      updatedControlPointVector
      (let [currentControlPoint (first controlPointVector)
            updatedControlPoint (assoc currentControlPoint :xIndex (inc currentIndex))]
        (recur (inc currentIndex)
               (conj updatedControlPointVector updatedControlPoint)
               (rest controlPointVector))))))

(defrecord ControlPoint [x y xIndex])

(defn randomControlPoint
  [x]
  (ControlPoint. (randomDoubleInARange defaultXMin defaultXMax)
                 (randomDoubleInARange defaultYMin defaultYMax)
                 x))

(defn randomVectorOfControlPoints
  [count]
  (loop [controlPoints []
         currentCount count
         x 0]
    (if (= 0 currentCount)
      controlPoints
      (recur (conj controlPoints (randomControlPoint x))
             (dec currentCount)
             (inc x)))))

(defrecord BezierCurve [controlPointVector fitness])

(defn randomBezierCurve
  [controlPointCount]
  (BezierCurve. (randomVectorOfControlPoints controlPointCount) 9999999))
