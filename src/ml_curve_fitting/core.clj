(ns binary-association-simulator.core
  (:require [ml_curve_fitting.GeneticAlgorithm :as GA])
  (:gen-class))

(defn -main
  "Starts the Genetic Algorithm."
  [& args]
  (GA/evolve))
