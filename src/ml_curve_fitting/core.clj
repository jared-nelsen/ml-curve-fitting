(ns ml-curve-fitting.core
  (:require [ml-curve-fitting.GeneticAlgorithm :as GA])
  (:gen-class))

(defn -main
  "Starts the Genetic Algorithm."
  [& args]
  (GA/evolveP))
