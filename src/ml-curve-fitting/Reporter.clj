
(ns ml-curve-fitting.Reporter)

(defn findBestFitness
  "Returns the hamming distance value of the most fit member of
   the given population."
  [population]
  (loop [bestFitness (Integer/MAX_VALUE)
         pop population]
    (if (empty? pop)
      bestFitness
      (let [current (first pop)
            currFitness (get current :fitness)]
        (if (< currFitness bestFitness)
          (recur currFitness (rest pop))
          (recur bestFitness (rest pop)))))))

(defn detectNewGlobalBest
  "Detects if a new global best has been found."
  [context newBestFitness]
  (let [#break currentGlobalBest (get context :globalBestFitness)]
    (if (< newBestFitness currentGlobalBest)
      newBestFitness
      currentGlobalBest)))

(defn report
  [context]
  (let [generation (get context :generation)
        population (get context :population)
        bestFitness (findBestFitness population)
        globalBest (detectNewGlobalBest context bestFitness)
        avgFitness (calculateAvgFitness population)]
    (do 
      (println (str "Generation: " generation
                    ", Best Fitness: " bestFitness
                    ", Avg Fitness: " avgFitness
                    ", Global Best: " globalBest))
      (assoc context
             :generation (inc (get context :generation))
             :bestFitness bestFitness
             :globalBestFitness globalBest
             :avgFitness avgFitness))))
