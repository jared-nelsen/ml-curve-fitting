(ns ml-curve-fitting.Data)

(defn randomDoubleInARange
  [low high]
  (+ low (* (rand) ( - high low))))

(def pointMinY -150.0)
(def pointMaxY 150.0)
(def xInterval 75)

(defrecord Point [x y])

(defn generateRandomPoint
  [x]
  (Point. x (randomDoubleInARange pointMinY pointMaxY)))

(defn generateVectorOfRandomPoints
  [length]
  (loop [vectorOfPoints []
         remainingPoints length
         xIndex 0]
    (if (= 0 remainingPoints)
      vectorOfPoints
      (recur (conj vectorOfPoints (generateRandomPoint xIndex))
             (dec remainingPoints)
             (+ xIndex xInterval)))))

(defrecord PointsToFit [points])

(defn generatePointsToFit
  [count]
  (PointsToFit. (generateVectorOfRandomPoints count)))
