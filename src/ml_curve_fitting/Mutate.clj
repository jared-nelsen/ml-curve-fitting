
(ns binary-association-simulator.Mutate
  (:require [binary-association-simulator.BinaryAssociator :as binary-associator]
            [binary-association-simulator.GeneticAlgorithm :as GA]))

;;-----------------------------------------------------------------------------------
;; Add and Remove
;;-----------------------------------------------------------------------------------

(defn addItemToVectorAtIndex
  "Adds the given item to the given vector at the given index."
  [vector item index]
  (let [size (count vector)
        precedent (subvec vector 0 index)
        precedent (conj precedent item)
        antecedent (subvec vector index size)]
    (into precedent antecedent)))

(defn removeItemFromVectorAtIndex
  "Removes the item that resides at the given index from the
   given vector."
  [vector index]
  (vec (concat (subvec vector 0 index) (subvec vector (inc index)))))

(defn addTransformationToTransformationVector
  "Adds a random Transform to a random index in the given
   Transformation Vector."
  [transformationVector]
  (let [transforms (get transformationVector :transformations)
        newRandomTransform (binary-associator/randomTransform)
        addIndex (rand (count transforms))
        updatedTransforms (addItemToVectorAtIndex transforms
                                                  newRandomTransform
                                                  addIndex)]
    (assoc transformationVector :transformations updatedTransforms)))

(defn addTransformationsToTransformationVector
  "Adds N Transforms to the given Transformation Vector where
   N is any of 0 to transformationAddRemoveCountMax according
   to the mutation rate."
  [context transformationVector mutationRate]
  (if (<= (rand) mutationRate)
      (let [addRemoveMax (get context :transformationAddRemoveCountMax)]
        (loop [count (rand-int (inc addRemoveMax))
               transformationVector transformationVector]
          (if (= 0 count)
            transformationVector
            (recur (dec count)
                   (addTransformationToTransformationVector transformationVector)))))
      transformationVector))

(defn removeTransformationFromTransformationVector
  "Randomly removes a Transform from the given Transformation Vector."
  [transformationVector]
  (let [transforms (get transformationVector :transformations)
        removeIndex (rand-int (count transforms))
        updatedTransforms (removeItemFromVectorAtIndex transforms
                                                       removeIndex)]
    (assoc transformationVector :transformations updatedTransforms)))

(defn removeTransformationsFromTransformationVector
  "Removes N Transforms from the given Transformation Vector where
   N is any of 0 to transformationAddRemoveCountMax according to
   the mutation rate."
  [context transformationVector mutationRate]
  (if (<= (rand) mutationRate)
    (let [addRemoveMax (get context :transformationAddRemoveCountMax)
          numTransforms (count transformationVector)
          count (rand-int (inc addRemoveMax))]
      (if (and (>= numTransforms 3) ;; Dont remove if 3 or less Transforms
               (> count (dec numTransforms))) ;; Dont remove all of them
        (loop [count count
               transformationVector transformationVector]
          (if (= 0 count)
            transformationVector
            (recur (dec count)
                   (removeTransformationFromTransformationVector transformationVector))))
        transformationVector))
    transformationVector))

;;----------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------
;; Operators and Data
;;-----------------------------------------------------------------------------------

(defn mutateData
  "Mutates the data in the given Transform."
  [transform mutationRate]
  (if (<= (rand) mutationRate)
    (let [newData (rand-int Integer/MAX_VALUE)]
      (assoc transform :data newData))
    transform))

(defn mutateOperation
  "Mutates the operation in the given Transform according to the mutation rate."
  [transform mutationRate]
  (if (<= (rand) mutationRate)
    (let [newOperation (rand-int 10)]
      (assoc transform :operation newOperation))
    transform))

(defn mutateBitOperationIndex
  "Mutates the operation index for single bit operations."
  [transform mutationRate]
  (if (<= (rand) mutationRate)
    (let [newBitOperationIndex (rand-int 32)]
      (assoc transform :bitOperationIndex newBitOperationIndex))
    transform))

(defn mutateOperationAndDataOfTransforms
  "Mutates the data and operation in each Transform in the given Transformation Vector
   according to the mutation rate."
  [transformationVector mutationRate]
  (loop [transforms (get transformationVector :transformations)
         mutatedTransforms []]
    (if (empty? transforms)
      (assoc transformationVector :transformations mutatedTransforms)
      (let [mutatedTransform (mutateData (first transforms) mutationRate)
            mutatedTransform (mutateOperation mutatedTransform mutationRate)
            mutatedTransform (mutateBitOperationIndex mutatedTransform mutationRate)]
        (recur (rest transforms) (conj mutatedTransforms mutatedTransform))))))

;;----------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------
;; Main Mutation Function
;;-----------------------------------------------------------------------------------

(defn mutateTransformationVector
  "Mutates the given Transformation Vector."
  [context transformationVector mutationRate]
  (let [tv (addTransformationsToTransformationVector context
                                                     transformationVector
                                                     mutationRate)
        tv (removeTransformationsFromTransformationVector context
                                                          transformationVector
                                                          mutationRate)
        tv (mutateOperationAndDataOfTransforms transformationVector
                                               mutationRate)]
    tv))

(defn mutatePopulationMember
  "Mutates the given population using the method indicated
   by the type of the population members."
  [context populationMember populationType mutationRate]
  (cond
    (= 1 populationType) (mutateTransformationVector context
                                                     populationMember
                                                     mutationRate)
    ;;CASE OTHER THAN TRANSFORMATION VECTOR
    ))

(defn mutate
  "Takes in the algortihm context and mutates all population
   members according to the mutation rate."
  [context]
  (let [population (get context :population)
        populationType (get context :populationType)
        mutationRate (get context :mutationRate)]
    (loop [pop population
           newPop []]
      (if (empty? pop)
        (assoc context :population newPop)
        (let [popMember (first pop)
              mutatedPopMember (mutatePopulationMember context
                                                       popMember
                                                       populationType
                                                       mutationRate)]
          (recur (rest pop) (conj newPop mutatedPopMember)))))))
