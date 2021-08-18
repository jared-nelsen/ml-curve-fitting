
(ns ml_curve_fitting.BezierCurve)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

(defrecord ControlPoint [x y])

(defn randomControlPoint
  [x]
  (ControlPoint. x (randomDoubleInARange defaultYMin defaultYMax)))

(defn randomVectorOfControlPoints
  [count]
  (loop [controlPoints []
         currentCount count
         x 1]
    (if (= 0 currentCount)
      controlPoints
      (recur (conj controlPoints (randomControlPoint x))
             (dec currentCount)
             (inc x)))))

(defrecord BezierCurve [controlPointVector fitness])

(defn randomBezierCurve
  [controlPointCount]
  (BezierCurve. (randomVectorOfControlPoints controlPointCount) 9999999))
