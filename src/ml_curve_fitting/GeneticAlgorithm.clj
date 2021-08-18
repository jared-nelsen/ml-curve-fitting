
(ns binary-association-simulator.GeneticAlgorithm
  (:require [binary-association-simulator.BinaryAssociator :as binary-associator]
            [binary-association-simulator.Data :as data]
            [binary-association-simulator.Select :as select]
            [binary-association-simulator.Crossover :as crossover]
            [binary-association-simulator.Mutate :as mutate]
            [binary-association-simulator.Evaluation :as evaluate]
            [binary-association-simulator.Reporter :as reporter]))

(def populationCount 1000)
(def transformationVectorInputOutputPairs 3)
(def mutationRate 0.05)
(def crossoverRate 0.8)

(defn generateTransformationVectorPopulation
  []
  (loop [remaining populationCount
         population []]
    (if (= 0 remaining)
      population
      (recur (dec remaining) (conj population
                                   (binary-associator/randomTransformationVector))))))

(defn generateTransformationVectorEvolutionContext
  []
  {:data (data/generateTransformationVectorSimulationData
          transformationVectorInputOutputPairs)
   :population (generateTransformationVectorPopulation)
   :populationCount populationCount
   :populationType 1
   :generation 0
   :bestFitness (Integer/MAX_VALUE)
   :globalBestFitness (Integer/MAX_VALUE)
   :avgFitness (Double/MAX_VALUE)
   :mutationRate mutationRate
   :transformationAddRemoveCountMax 10
   :crossoverRate crossoverRate})

(defn evolve
  "Performs the Genetic Algorithm on the generated algorithm context."
  []
  (loop [generations 99999999
         algorithmContext (generateTransformationVectorEvolutionContext)]
    (if (= 0 generations)
      (System/exit 0)
      (recur (dec generations)
             (-> algorithmContext
                 select/selectMembers
                 crossover/crossover
                 mutate/mutate
                 evaluate/evaluate
                 reporter/report)))))
