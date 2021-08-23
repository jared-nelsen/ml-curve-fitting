(ns ml_curve_fitting.Evaluation)

(defn integerToThirtyTwoBitString
  "Takes any integer and returns its string representation in 32 bits"
  [x]
  (let [bin (Integer/toBinaryString x)
        diff (- 32 (count bin))
        prefix (clojure.string/join (repeat diff "0"))]
    (str prefix bin)))

;; This is a potential killer bug when compiling compute graphs
;; in future. Need to reevaluate if this is the right strategey.
;; Can run some automated tests with the operate function in
;; BinaryAssociator to try to find the bug. For now this prevents
;; overflow errors.
(defn safeIntegerToThirtyTwoBitString
  "Takes any integer and returns its string representation in 32 bits.
   In the case that the given integer is outside the bounds of valid
   integer values then the binary representation of the max integer
   in Java is returned."
  [x]
  (try
    (integerToThirtyTwoBitString x)
    (catch Exception e (if (>= x 0)
                         (integerToThirtyTwoBitString (Integer/MAX_VALUE))
                         (integerToThirtyTwoBitString (Integer/MIN_VALUE))))))

(defn hammingDistance
  "Calculates the hamming distance between x and y."
  [x y]
  (let [xS (safeIntegerToThirtyTwoBitString x)
        yS (safeIntegerToThirtyTwoBitString y)]
    (loop [diffs 0
           xS xS
           yS yS]
      (if (empty? xS)
        diffs
        (let [xChar (first xS)
              yChar (first yS)]
          (if (not= xChar yChar)
            (recur (inc diffs) (rest xS) (rest yS))
            (recur diffs (rest xS) (rest yS))))))))

;; (defn evaluateTransformationVectorOnData
;;   "Evaluates the given Transformation Vector using the given data and
;;    sets the resultant fitnesses on it."
;;   [transformationVector data]
;;   (loop [inputData (get data :inputFrame)
;;          outputData (get data :outputFrame)
;;          hammingDistanceTotal 0]
;;     (if (empty? inputData)
;;       (assoc transformationVector
;;              :hammingDistanceTotal hammingDistanceTotal)
;;       (let [in (first inputData)
;;             out (first outputData)
;;             actual (binary-associator/propogateThroughTransformationVector
;;                     transformationVector
;;                     in)]
;;         (if (not= out actual)
;;           (recur (rest inputData)
;;                  (rest outputData)
;;                  (+ hammingDistanceTotal (hammingDistance out actual)))
;;           (recur (rest inputData)
;;                  (rest outputData)
;;                  hammingDistanceTotal))))))

(defn evaluateTransformationVector
  [transformationVector algorithmContext]
  (evaluateTransformationVectorOnData transformationVector
                                      (get algorithmContext :data)))

(defn evaluatePopulation
  "Evaluates the population found in the
   given algorithm context with the given evaluation function."
  [algorithmContext evaluationFunction]
  (let [populationCount (get algorithmContext :populationCount)]
    (loop [population (get algorithmContext :population)
           newPopulation []
           bestHammingDistanceScore 9999999
           hammingDistanceSum 0]
      (if (empty? population)
        (let [hammingDistanceAverage (/ hammingDistanceSum populationCount)]
          (assoc algorithmContext
                 :population newPopulation
                 :bestFitness bestHammingDistanceScore
                 :avgFitness hammingDistanceAverage))
        (let [member (first population)
              evaluatedMember (evaluationFunction member
                                                  algorithmContext)
              updatedPopulation (conj newPopulation evaluatedMember)
              hammingDistance (get evaluatedMember :hammingDistanceTotal)
              newBestHamming (min hammingDistance bestHammingDistanceScore)]
          (recur (rest population)
                 updatedPopulation
                 newBestHamming
                 (+ hammingDistance hammingDistanceSum)))))))

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
