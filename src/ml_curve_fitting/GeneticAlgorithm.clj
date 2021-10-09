
(ns ml-curve-fitting.GeneticAlgorithm
  (:require [ml-curve-fitting.BezierCurve :as bCurve]
            [ml-curve-fitting.Data :as data]
            [ml-curve-fitting.Select :as select]
            [ml-curve-fitting.Crossover :as crossover]
            [ml-curve-fitting.Mutate :as mutate]
            [ml-curve-fitting.Evaluation :as evaluate]
            [ml-curve-fitting.Reporter :as reporter]
            [ml-curve-fitting.Animation :as animation]))

(def coreCount (- (.availableProcessors (Runtime/getRuntime)) 2))
(def acPopMultiplicationFactor 1) ;; How many times the coreCount sized population should be multiplied
(def acPopulationSize (* coreCount acPopMultiplicationFactor))
(def subPopulationCount 20) ;;The count of population members for each core


(def populationCount 1)
(def positionMutationRate 0.8)
(def addRemoveMutationRate 0.2)
(def crossoverRate 0.9)

(def controlPointCount 5)
(def pointsToFitCount 5)

(defn generateBezierCurvePopulation
  "Generates a population of Bezier Curves."
  [memberCount]
  (loop [remaining memberCount
         population []]
    (if (= 0 remaining)
      population
      (recur (dec remaining) (conj population
                                   (bCurve/randomBezierCurve controlPointCount))))))

(defn generateAlgorithmContext
  "Generates the Algorithm Context."
  [populationCount data]
  {:data data
   :population (generateBezierCurvePopulation populationCount)
   :populationCount populationCount
   :indexOfFittestMember 0
   :generation 0
   :bestFitness (Integer/MAX_VALUE)
   :globalBestFitness (Integer/MAX_VALUE)
   :avgFitness (Double/MAX_VALUE)
   :positionMutationRate positionMutationRate
   :addRemoveMutationRate addRemoveMutationRate
   :crossoverRate crossoverRate
   :bCurveDrawingInterval 0.05})

(defn generateNAlgorithmContexts
  "Generates N Algorithm Contexts for parallel evaluation. Passes in how many
   members the population in each Algorithm Context should contain."
  [acPopCount subPopCount data]
  (loop [acPopCount acPopCount
         contexts []]
    (if (= 0 acPopCount)
      contexts
      (let [newMember (generateAlgorithmContext subPopCount data)]
        (recur (dec acPopCount) (conj contexts newMember))))))


(defn evolve
  "Performs the Genetic Algorithm on the generated Algorithm Context."
  []
  (let [data (data/generatePointsToFit pointsToFitCount)]
    (loop [generations 99999999
           algorithmContext (generateAlgorithmContext populationCount data)]
      (if (= 0 generations)
        (System/exit 0)
        (recur (dec generations)
               (-> algorithmContext
                   select/selectMembers
                   crossover/crossover
                   mutate/mutate
                   evaluate/evaluate
                   reporter/report))))))

(defn evolveP
  "Performs the Genetic Algorithm on the generated population of Algorithm Contexts."
  []
  (let [data (data/generatePointsToFit pointsToFitCount)]
    (loop [generations 99999999
           contexts (generateNAlgorithmContexts acPopulationSize
                                                subPopulationCount
                                                data)]
      (if (= 0 generations)
        (System/exit 0)
        (recur (dec generations)
               (-> contexts
                   select/selectMembersP
                   crossover/crossoverP
                   mutate/mutateP
                   evaluate/evaluateP
                   reporter/reportP
                   animation/updateP))))))
