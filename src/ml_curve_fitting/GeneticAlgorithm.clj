
(ns ml-curve-fitting.GeneticAlgorithm
  (:require [ml-curve-fitting.BezierCurve :as bCurve]
            [ml-curve-fitting.Data :as data]
            [ml-curve-fitting.Select :as select]
            [ml-curve-fitting.Crossover :as crossover]
            [ml-curve-fitting.Mutate :as mutate]
            [ml-curve-fitting.Evaluation :as evaluate]
            [ml-curve-fitting.Reporter :as reporter]))

(def populationCount 10)
(def positionMutationRate 0.8)
(def addRemoveMutationRate 0.3)
(def crossoverRate 0.8)
(def controlPointCount 9)

(defn generateBezierCurvePopulation
  []
  (loop [remaining populationCount
         population []]
    (if (= 0 remaining)
      population
      (recur (dec remaining) (conj population
                                   (bCurve/randomBezierCurve controlPointCount))))))

(defn generateAlgorithmContext
  []
  {:data (data/generatePointsToFit)
   :population (generateBezierCurvePopulation)
   :populationCount populationCount
   :indexOfFittestMember -1
   :generation 0
   :bestFitness (Integer/MAX_VALUE)
   :globalBestFitness (Integer/MAX_VALUE)
   :avgFitness (Double/MAX_VALUE)
   :positionMutationRate positionMutationRate
   :addRemoveMutationRate addRemoveMutationRate
   :crossoverRate crossoverRate
   :bCurveDrawingInterval 0.05})

(defn evolve
  "Performs the Genetic Algorithm on the generated algorithm context."
  []
  (loop [generations 99999999
         algorithmContext (generateAlgorithmContext)]
    (if (= 0 generations)
      (System/exit 0)
      (recur (dec generations)
             (-> algorithmContext
                 select/selectMembers
                 crossover/crossover
                 mutate/mutate
                 evaluate/evaluate
                 reporter/report)))))
