
(ns binary-association-simulator.Select
  (:require [binary-association-simulator.BinaryAssociator :as binary-associator]
            [binary-association-simulator.GeneticAlgorithm :as GA]))

(defn selectFittestPopulationMemberHammingDistance
  "Selects the fittest member of the given
   population by Hamming Distance."
  [population]
  (loop [population population
         fittestMember (first population)]
      (if (empty? population)
        fittestMember
        (let [current (first population)
              currentHamming (get current :hammingDistanceTotal)
              currentBest (get fittestMember :hammingDistanceTotal)]
          (if (< currentHamming currentBest)
            (recur (rest population) current)
            (recur (rest population) fittestMember))))))

(defn randomlySelectNPopulationMembers
  "Randomly Selects N population members. Avoids
   selecting the same member more than once."
  [population N]
  (loop [N N
         availableSelectionIndeces (take (count population) (range))
         selectedMembers []]
    (if (= 0 N)
      selectedMembers
      (let [selectedMemberIndex (rand-nth availableSelectionIndeces)
            selectedMember (get population selectedMemberIndex)
            remainingAvailableIndeces (filter (fn [x] (not= x selectedMemberIndex))
                                              availableSelectionIndeces)]
        (recur (dec N)
               remainingAvailableIndeces
               (conj selectedMembers selectedMember))))))

(defn tournamentSelection
  "Performs tournament selection on the given
   population by selecting N unique members to
   compete. The fittest member is selected as
   the winner."
  [population tournamentSize]
  (let [selectedTournamentMembers (randomlySelectNPopulationMembers
                                   population
                                   tournamentSize)
        fittestMember (selectFittestPopulationMemberHammingDistance
                       selectedTournamentMembers)]
    fittestMember))

(defn selectParents
  "Selects two parents by doing tournament selection
   twice on the given population."
  [population tournamentSize]
  (loop [remainingParents 2
         selectedParents []]
    (if (= 0 remainingParents)
      selectedParents
      (let [selectedParent (tournamentSelection population
                                                tournamentSize)]
        (recur (dec remainingParents) (conj selectedParents selectedParent))))))

(defn selectMembers
  "Selects a new population of population members to
   be crossed over. Returns a population of N * 2
   members where member n will be crossed over with
   member n + 1 in the Crossover routine. Tournament
   pool size is determined as 2/10ths of the population."
  [algorithmContext]
  (let [populationCount (get algorithmContext :populationCount)
        population (get algorithmContext :population)
        tournamentSize (int (* populationCount 0.5))]
    (loop [newPopulation []
           remainingSelects populationCount]
      (if (= 0 remainingSelects)
        (assoc algorithmContext :population newPopulation)
        (let [selectedParents (selectParents population
                                             tournamentSize)]
          (recur (into [] (concat newPopulation selectedParents))
                 (dec remainingSelects)))))))
