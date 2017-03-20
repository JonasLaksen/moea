(ns evolution.evolution
  (:gen-class)
  (:require [evolution.segmentation :refer :all])
  (:require [evolution.utils :refer :all])
  (:require [clojure.tools.trace :refer :all])
  (:require [clojure.math.numeric-tower :refer :all])
)

(defn- select
  "Tournament selection"
  [n tournament-size evaluate solutions]
    (if (or (zero? n) (= solutions [])) 
      [] 
      (let [chosen (->> solutions 
                        (shuffle)
                        (take tournament-size)
                        (apply max-key evaluate))] 
        (concat 
         [chosen] 
         (select (- n 1) tournament-size evaluate (remove-once  #(= %1 chosen) solutions))))))

(defn- reproduce
  [crossover mutate offspringCount parents]
  (for [x (range offspringCount)] 
    (->> (crossover (rand-nth parents) (rand-nth parents))
         (mutate))))

(defn- evolve
  [survival-rate evaluate crossover mutate solutions]
    (let [pop-size       (count solutions)
          survival-count (int (* survival-rate pop-size))
          winners        (select survival-count (/ pop-size 2) evaluate solutions)
          offspring (reproduce crossover mutate 
                                      (- pop-size survival-count) winners)] 
      (trace "---------------------")
      (trace "Best solution" (apply max (map evaluate (concat winners offspring))))
      (trace "Mean" (mean (map evaluate (concat winners offspring))))
      (concat winners offspring)))

(defn simulate-evolution
  [it survival-rate evaluate crossover mutate initial-solutions]
  (time 
   (let [result (reduce 
                 (fn [x y] 
                   (evolve survival-rate evaluate crossover mutate x)) 
                 initial-solutions 
                 (take it (repeat 0)))] 
     (sort-by evaluate result))))
