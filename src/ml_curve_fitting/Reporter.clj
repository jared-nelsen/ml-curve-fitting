
(ns ml-curve-fitting.Reporter)

(defn findFittestPopulationMemberIndex
  "Returns the index of the fittest member in the population."
  [population]
  (loop [bestFitness (Integer/MAX_VALUE)
         fittestMemberIndex 0
         currentIndex 0
         pop population]
    (if (empty? pop)
      bestFitness
      (let [current (first pop)
            currFitness (get current :fitness)]
        (if (< currFitness bestFitness)
          (recur currFitness currentIndex (inc currentIndex) (rest pop))
          (recur bestFitness fittestMemberIndex (inc currentIndex) (rest pop)))))))

(defn detectNewGlobalBest
  "Detects if a new global best has been found."
  [context newBestFitness]
  (let [currentGlobalBest (get context :globalBestFitness)]
    (if (< newBestFitness currentGlobalBest)
      newBestFitness
      currentGlobalBest)))

(defn report
  [context]
  (let [generation (get context :generation)
        population (get context :population)
        indexOfFittestMember (findFittestPopulationMemberIndex population)
        bestFitness (get (nth population indexOfFittestMember) :fitness)
        globalBest (detectNewGlobalBest context bestFitness)
        avgFitness 1] ;;temporary
    (do 
      (println (str "Generation: " generation
                    ", Best Fitness: " bestFitness
                    ", Avg Fitness: " avgFitness
                    ", Global Best: " globalBest))
      (assoc context
             :generation (inc (get context :generation))
             :bestFitness bestFitness
             :globalBestFitness globalBest
             :indexOfFittestMember indexOfFittestMember
             :avgFitness avgFitness))))
