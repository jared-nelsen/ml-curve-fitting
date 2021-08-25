(ns ml-curve-fitting.Data)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

(def pointMinY -10.0)
(def pointMaxY 10.0)
(def defaultPointsToFit 5)

(defrecord Point [x y])

(defn generateRandomPoint
  [x]
  (Point. x (randomDoubleInARange pointMinY pointMaxY)))

(defn generateVectorOfRandomPoints
  [length]
  (loop [vectorOfPoints []
         remainingPoints length
         xIndex 1]
    (if (= 0 remainingPoints)
      vectorOfPoints
      (recur (conj vectorOfPoints (generateRandomPoint xIndex))
             (dec remainingPoints)
             (inc xIndex)))))

(defrecord PointsToFit [points])

(defn generatePointsToFit
  []
  (PointsToFit. (generateVectorOfRandomPoints defaultPointsToFit)))
