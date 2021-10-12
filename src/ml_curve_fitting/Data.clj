(ns ml-curve-fitting.Data)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

(def pointMinY -150.0)
(def pointMaxY 150.0)
(def pointMinX -150.0)
(def pointMaxX 150.0)

(defrecord Point [x y])

(defn generateRandomPoint
  []
  (Point. (randomDoubleInARange pointMinX pointMaxX)
          (randomDoubleInARange pointMinY pointMaxY)))

(defn generateVectorOfRandomPoints
  [length]
  (loop [vectorOfPoints []
         remainingPoints length
         xIndex 0]
    (if (= 0 remainingPoints)
      vectorOfPoints
      (recur (conj vectorOfPoints (generateRandomPoint))
             (dec remainingPoints)
             (inc xIndex)))))

(defrecord PointsToFit [points])

(defn generatePointsToFit
  [count]
  (PointsToFit. (generateVectorOfRandomPoints count)))
