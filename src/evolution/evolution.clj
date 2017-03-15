(ns evolution.evolution
  (:gen-class)
  (:require [clojure.tools.trace :refer :all])
)

(defn- select
  "Tournament selection"
  [n tournament-size evaluate solutions]
  (if (zero? n)
    (do 
      (trace (map evaluate solutions))
      [])
    (let [chosen (->> solutions
          (shuffle)
          (take tournament-size)
          (apply max-key evaluate))]
        (trace (evaluate chosen))
        (concat 
         [chosen] 
         (select (- n 1) tournament-size evaluate (remove (fn [x] (= x chosen)) solutions))))
))

(defn- reproduce
  [crossover mutate offspringCount parents]
  (for [x (range offspringCount)] 
    (->> (crossover (rand-nth parents) (rand-nth parents))
         (mutate))))

(defn- evolve
  [survival-rate evaluate crossover mutate solutions]
    (let [pop-size       (count solutions)
          survival-count (int (* survival-rate pop-size))
          winners        (select survival-count (/ pop-size 10) evaluate solutions)
          offspring (reproduce crossover mutate 
                                      (- pop-size survival-count) winners)]
      (concat winners offspring)
           ))

(defn simulate-evolution
  [it survival-rate evaluate crossover mutate initial-solutions]
  (let [result (reduce (fn [x y] 
                         (evolve survival-rate evaluate crossover mutate x))
                       initial-solutions (take it (repeat 0)))]
    result))
