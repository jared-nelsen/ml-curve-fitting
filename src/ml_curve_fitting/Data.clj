(ns ml-curve-fitting.Data)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

(def pointMinY -1.0)
(def pointMaxY 1.0)

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
  [count]
  (PointsToFit. (generateVectorOfRandomPoints count)))
