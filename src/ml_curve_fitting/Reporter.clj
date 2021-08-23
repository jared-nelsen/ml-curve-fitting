
(ns ml_curve_fitting.Reporter)

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
  (let [currentGlobalBest (get context :globalBestFitness)]
    (if (< newBestFitness currentGlobalBest)
      newBestFitness
      currentGlobalBest)))

(defn calculateAvgFitness
  [population]
  (loop [pop population
         fitnessSum 0]
    (if (empty? pop)
      (double (/ fitnessSum (count population)))
      (let [current (first pop)
            currentFitness (get current :fitness)]
        (recur (rest pop) (+ fitnessSum currentFitness))))))

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
