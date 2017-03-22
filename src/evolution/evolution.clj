(ns evolution.evolution
  (:gen-class)
  (:require [evolution.segmentation :refer :all])
  (:require [evolution.utils :refer :all]
            [medley.core :refer [distinct-by]]
            [clojure.core.matrix :refer [distance mget row-count column-count]])
  (:require [clojure.tools.trace :refer :all])
  (:require [clojure.math.numeric-tower :refer :all]))

(defn >>
  [evaluate a b]
  (every? identity (map > (evaluate a) (evaluate b))))

(defn dominating
  [obj x xs]
  (filter #(>> obj x %) xs))

(defn dominated-by
  [obj x xs]
  (filter #(>> obj % x) xs))

(defn non-dominated
  [evaluate xs]
  (filter (fn [x] (not (some #(>> evaluate %1 x) xs))) xs))

(defn reproduce 
  [crossover mutate a b]
  (->> (crossover a b)
       (mutate)))

(defn unique
  [evaluate xs]
  (distinct-by #(map str (map int (evaluate %))) xs)
)

(defn strength
  [obj x xs]
  (count (dominating obj x xs)))

(defn raw-fitness
  [obj x xs]
  (let [strengths (zipmap xs (map #(strength obj % xs) xs))
        >x (dominated-by obj x xs)]
    (reduce + (map #(get strengths %) >x))))

(defn kth-nearest-neighbor
  [obj k x xs]
  (nth (sort-by #(distance (obj x) (obj %)) xs) k)
)

(defn- density
  [obj x xs]
  (/ 1 (+ 2 (distance (obj x) (obj (kth-nearest-neighbor obj (sqrt (count xs)) x xs))))))


(defn fitnessz
  [obj x xs]
  (+ (raw-fitness obj x xs) (density obj x xs)))

(defn truncate
  [obj n xs]
  (if (= n 0)
    []
    (let [sorted (sort-by #(fitnessz obj % xs) xs)]
      (concat [(first sorted)] (truncate obj (- n 1) (rest sorted))))))

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
         (select (- n 1) tournament-size evaluate solutions)))))

(defn- reproduce
  [crossover mutate offspringCount parents]
  (for [x (range offspringCount)] 
    (->> (crossover (rand-nth parents) (rand-nth parents))
         (mutate))))

(defn evolve
  [it obj crossover mutate population archive archive-size population-size]
  (if (= it 0)
    (non-dominated obj (concat population archive))
    (let [all (unique obj (concat population archive))
          non-dominated (non-dominated obj all)
          new-archive (if (= (count non-dominated) archive-size)
                        non-dominated
                        (if (> (count non-dominated) archive-size)
                          (truncate obj archive-size non-dominated)
                          (take archive-size (sort-by #(fitnessz obj % all) all))))
          selected (select population-size 2 #(fitnessz obj % new-archive) new-archive)
          offspring (reproduce crossover mutate population-size selected)]
      (trace (map obj new-archive))
      (evolve (- it 1) obj crossover mutate offspring new-archive archive-size population-size))
))

(defn simulate-evolution
  [it objective-function crossover mutate initial-solutions]
  (time 
   (let [result (evolve 
                 it 
                 objective-function 
                 crossover 
                 mutate 
                 initial-solutions 
                 [] 
                 (count initial-solutions)
                 (count initial-solutions))]
     (take 5 (shuffle result)))))
