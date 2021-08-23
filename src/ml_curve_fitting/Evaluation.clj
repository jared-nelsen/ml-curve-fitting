(ns ml_curve_fitting.Evaluation)

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
         x (- n (+ k 1))]
    (if (> x n)
      coeff
      (recur (* coeff x)
             (inc x)))))

(defn C
  "Calculates the Binary Coefficient for N, K."
  [n k]
  (Cb (Ca 1 n k) k))

(defn generatePointOnBezierCurve
  "Generates the X,Y point on the given Bezier curve at the given X."
  [bCurve x])




(defn evaluatePopulation
  "Evaluates the population found in the given algorithm context with the
   given evaluation function."
  [algorithmContext evaluationFunction]
  (let [populationCount (get algorithmContext :populationCount)]
    (loop [population (get algorithmContext :population)
           newPopulation []
           bestFitness (Integer/MAX_VALUE)
           fitnessSum 0]
      (if (empty? population)
        (let [fitnessAverage (/ fitnessSum populationCount)]
          (assoc algorithmContext
                 :population newPopulation
                 :bestFitness bestFitness
                 :avgFitness fitnessAverage))
        (let [member (first population)
              evaluatedMember (evaluationFunction member
                                                  algorithmContext)
              updatedPopulation (conj newPopulation evaluatedMember)
              currentMemberFitness (get evaluatedMember :fitness)
              newBestFitness (min currentMemberFitness bestFitness)]
          (recur (rest population)
                 updatedPopulation
                 newBestFitness
                 (+ currentMemberFitness fitnessSum)))))))

(defn evaluateTransformationVectorPopulation
  "Evaluates the population of Transformation Vectors found in
   the given algorithm context."
  [algorithmContext]
  (let [evalFunction evaluateTransformationVector]
    (evaluatePopulation algorithmContext evalFunction)))

(defn evaluate
  "Evaluates the population in the given algorithm context."
  [algorithmContext]
  (let [populationType (get algorithmContext :populationType)]
    (if (= populationType 1)
      (evaluateTransformationVectorPopulation algorithmContext)
      ;;Pop type other than transformation vector
      )))
