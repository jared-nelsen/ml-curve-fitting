
(ns ml_curve_fitting.GeneticAlgorithm
  (:require [ml_curve_fitting.BezierCurve :as bCurve]
            [ml_curve_fitting.Data :as data]
            [ml_curve_fitting.Select :as select]
            [ml_curve_fitting.Crossover :as crossover]
            [ml_curve_fitting.Mutate :as mutate]
            [ml_curve_fitting.Evaluation :as evaluate]
            [ml_curve_fitting.Reporter :as reporter]))

(def populationCount 1000)
(def mutationRate 0.05)
(def crossoverRate 0.8)

(defn generateBezierCurvePopulation
  []
  (loop [remaining populationCount
         population []]
    (if (= 0 remaining)
      population
      (recur (dec remaining) (conj population
                                   (bCurve/randomBezierCurve))))))

(defn generateAlgorithmContext
  []
  {:data (data/generatePointsToFit)
   :population (generateBezierCurvePopulation)
   :populationCount populationCount
   :generation 0
   :bestFitness (Integer/MAX_VALUE)
   :globalBestFitness (Integer/MAX_VALUE)
   :avgFitness (Double/MAX_VALUE)
   :mutationRate mutationRate
   :crossoverRate crossoverRate
   :bCurveDrawingInterval .05})

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
