(ns ml-curve-fitting.Evaluation
  (:import [java.util.concurrent Executors Executors]))

;; A Bezier curve is defined by its Control Points. To evaluate the given
;; Bezier Curve agaist the given Data we must generate the Y components
;; of the given Bezier Curve at the given X components of the Data to be
;; fit and calculate the error which is the absolute value of the differences
;; between the given Y components.
;; We would also like to calculate a lot of points on the line to draw it.
;;
;; Here is the psuedocode:
;;
;;     1. Generate a range of M points from 1 to N of the Bezier curve to
;;        draw.
;;     2. For each Point P in points to fit calculate the Y component of the
;;        point on the Bezier Curve that lies at the X of P.
;;     3. For each of the generated points calculate the absolute value of
;;        the difference between the Ys.
;;     4. Sum the absolute differences.

(defrecord Point [x y])

(defn Cb
  "Calculates the B component of C."
  [coeff k]
  (loop [coeff coeff
         x 1]
    (if (> x k)
      coeff
      (recur (/ coeff x)
             (inc x)))))

(defn Ca
  "Calculates the A component of C."
  [coeff n k]
  (loop [coeff coeff
         x (-' n (+' k 1))]
    (if (> x n)
      coeff
      (recur (*' coeff x)
             (inc x)))))

(defn C
  "Calculates the Binary Coefficient for N, K."
  [n k]
  (Cb (Ca 1 n k) k))

(defn generatePointOnBezierCurve
  "Generates the X,Y point on the given Bezier Curve at the given X."
  [controlPoints xt]
  (let [n (- (count controlPoints) 1)]
    (loop [x 0
           y 0
           i n]
      (if (= -1 i) ;;-1 or 0??
        (Point. x y)
        (let [controlPoint (nth controlPoints i)
              c (C n i)
              m1 (Math/pow (- 1 xt) (- n i))
              m2 (Math/pow xt i)
              bin (* c (* m1 m2))
              xi (+ x (* bin (get controlPoint :x)))
              yi (+ y (* bin (get controlPoint :y)))]
          (recur xi yi (dec i)))))))

(defn generateCorrespondingPointsOnBCurveGivenControlPoints
  "Generates a vector of Points that correspond with the X component of
   a given set of data along the Bezier Curve of the given Control Points."
  [controlPoints data]
  (loop [data data
         generatedPoints []]
    (if (or (empty? data) (empty? controlPoints))
      generatedPoints
      (let [dataPoint (first data)
            x (get dataPoint :x)
            newPoint (generatePointOnBezierCurve controlPoints x)]
        (recur (rest data) (conj generatedPoints newPoint))))))
 
(defn generateBCurvePointsOnIntervalP
  "Generates points along the given Bezier Curve at the given interval
   in parallel. 1 is perfectly divisible by the interval."
  [controlPoints interval]
  (let [r (vec (range -1.0 1.0 interval))]
    (pmap #(generatePointOnBezierCurve controlPoints %) r)))

(defn evaluateGeneratedBCurvePointsAgainstData
  "Calculates the sum of the absolute values of the differences between
   the Y coordinates for each X of each Data Point in the Data to fit."
  [bCurve data]
  (loop [controlPoints (get bCurve :controlPointVector)
         generatedPoints (generateCorrespondingPointsOnBCurveGivenControlPoints controlPoints
                                                                                data)
         errorSum 0]
    (if (empty? generatedPoints)
      errorSum
      (let [controlPoint (first controlPoints)
            controlY (get controlPoint :y)
            generatedPoint (first generatedPoints)
            generatedY (get generatedPoint :y)
            error (Math/abs (- generatedY controlY))]
        (recur (rest controlPoints)
               (rest generatedPoints)
               (+' errorSum error))))))

(defn evaluateBezierCurve
  "Evaluates and sets the fitness on the given Bezier Curve."
  [bCurve data]
  (assoc bCurve :fitness (evaluateGeneratedBCurvePointsAgainstData bCurve data)))

(defn evaluatePopulation
  "Evaluates the population in the algorithm context."
  [algorithmContext]
  (let [population (get algorithmContext :population)
        data (get-in algorithmContext [:data :points])]
    (loop [population population
           newPopulation []]
      (if (empty? population)
        newPopulation
        (let [member (first population)
              evaluatedMember (evaluateBezierCurve member data)]
          (recur (rest population)
                 (conj newPopulation evaluatedMember)))))))

(defn evaluatePopulationSingleThreaded
  "Evaluates the population of the given Algorithm Context on a single thread."
  [algorithmContext]
  (let [evaledPop (evaluatePopulation algorithmContext)]
        (assoc algorithmContext :population evaledPop)))

(defn evaluate
  "Evaluates the population in the given algorithm context."
  [algorithmContext]
  (evaluatePopulationSingleThreaded algorithmContext))

(defn evaluatePopulationMultiThreaded
  "Evaluates the given population of Algorithm Contexts."
  [acPop]
  (pmap evaluatePopulationSingleThreaded acPop))

(defn evaluateP
  "Evaluates the given population of Algorithm Contexts."
  [acPop]
  (evaluatePopulationMultiThreaded acPop))

