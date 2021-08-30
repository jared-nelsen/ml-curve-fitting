
(ns ml-curve-fitting.Reporter)

(defn findFittestPopulationMemberIndex
  "Returns the index of the fittest member in the population."
  [population]
  (loop [bestFitness (Integer/MAX_VALUE)
         fittestMemberIndex 0
         currentIndex 0
         pop population]
    (if (empty? pop)
      fittestMemberIndex
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

(defn findFittestAlgorithmContext
  "Finds and returns the fittest Algorithm Context in the given population."
  [acPop]
  (loop [fittestContext (first acPop)
         acPop acPop]
    (if (empty? acPop)
      fittestContext
      (let [currentAc (first acPop)
            currentBestFitness (:bestFitness currentAc)
            fittestDetectedFitness (:bestFitness fittestContext)]
        (if (< currentBestFitness fittestDetectedFitness)
          (recur currentAc (rest acPop))
          (recur fittestContext (rest acPop)))))))

(defn calculateStats
  "Calculates some key statistics about the state of the Algorithm."
  [context]
  (let [generation (get context :generation)
        population (get context :population)
        indexOfFittestMember (findFittestPopulationMemberIndex population)
        bestFitness (get (nth population indexOfFittestMember) :fitness)
        globalBest (detectNewGlobalBest context bestFitness)]
    (assoc context
             :generation (inc generation)
             :bestFitness bestFitness
             :globalBestFitness globalBest
             :indexOfFittestMember indexOfFittestMember)))

(defn report
  "Reports on the progress of the Genetic Algorithm."
  [context]
  (let [context (calculateStats context)
        generation (:generation context)
        bestFitness (:bestFitness context)
        globalBestFitness (:globalBestFitness context)]
    (do (println (str "Generation: " generation
                      ", Best Fitness: " bestFitness
                      ", Global Best Fitness: " globalBestFitness))
        context)))

(defn reportP
  "Reports on the progress of the Genetic Algorithm in parallel mode."
  [contexts]
  (let [contexts (pmap calculateStats contexts)
        fittestContext (findFittestAlgorithmContext contexts)
        generation (:generation fittestContext)
        bestFitness (:bestFitness fittestContext)
        globalBestFitness (:globalBestFitness fittestContext)]
    (do (println (str "P- Generation: " generation
                      ", Best Fitness: " bestFitness
                      ", Global Best Fitness: " globalBestFitness))
        contexts)))
