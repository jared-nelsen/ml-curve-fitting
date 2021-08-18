
(ns binary-association-simulator.Crossover
  (:require [binary-association-simulator.BinaryAssociator :as binary-associator]
            [binary-association-simulator.GeneticAlgorithm :as GA]))

(defn crossoverOperation
  "Assimilates the operation of Transformation A
   into Transformation B according to the crossover rate."
  [crossoverRate A B]
  (if (< (rand) crossoverRate)
    (assoc B :operation (get A :operation))
    B))

(defn crossoverBitOperationIndex
  "Assimilates the bit operation index of Transformation A
   into Transformation B according to the crossover rate."
  [crossoverRate A B]
  (if (< (rand) crossoverRate)
    (assoc B :bitOperationIndex (get A :bitOperationIndex))
    B))

(defn crossoverTransforms
  "Crosses over the two given Transforms."
  [crossoverRate transform1 transform2]
  (let [newTransform transform1
        newTransform (crossoverOperation crossoverRate
                                         transform2
                                         newTransform)
        newTransform (crossoverBitOperationIndex crossoverRate
                                                 transform2
                                                 newTransform)]
    newTransform))

(defn compareTransformationVectorTransformationsLength
  "Compares the given Transformation Vectors and returns the one
   that the given comparison function proves to be true. The
   acceptable comparison functions are: <= and >"
  [tv1 tv2 operator]
  (let [tv1Transformations (get tv1 :transformations)
        tv1Count (count tv1Transformations)
        tv2Transformations (get tv2 :transformations)
        tv2Count (count tv2Transformations)]
    (if (operator tv1Count tv2Count)
      tv1Transformations
      tv2Transformations)))

(defn crossoverGivenTransformationVectors
  "Crosses over the two given Transformation Vectors"
  [crossoverRate tv1 tv2]
  (let [tvTransforms1 (compareTransformationVectorTransformationsLength tv1
                                                                        tv2
                                                                        <=)
        tvTransforms2 (compareTransformationVectorTransformationsLength tv1
                                                                        tv2
                                                                        >)]
    (loop [tvt1 tvTransforms1
           tvt2 tvTransforms2
           newTransforms []]
      (if (empty? tvt1)
        (assoc tv1 :transformations newTransforms)
        (let [transformA (first tvt1)
              transformB (first tvt2)
              newTransform (crossoverTransforms crossoverRate
                                                transformA
                                                transformB)]
          (recur (rest tvt1)
                 (rest tvt2)
                 (conj newTransforms newTransform)))))))

(defn crossoverGivenMembers
  "Crosses over the given members according to the type of member."
  [algorithmContext member1 member2]
  (let [populationType (get algorithmContext :populationType)
        crossoverRate (get algorithmContext :crossoverRate)]
    (if (= 1 populationType)
      (crossoverGivenTransformationVectors crossoverRate
                                           member1
                                           member2)
      ;;CASE OTHER THAN TRANSFORMATION VECTOR
      )))

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
            newMember (crossoverGivenMembers algorithmContext
                                             member1
                                             member2)]
        (recur (rest (rest population))
               (conj newPopulation newMember))))))
