(ns evolution.evolution
  (:gen-class)
  (:require [evolution.segmentation :refer :all])
  (:require [evolution.utils :refer :all])
  (:require [clojure.tools.trace :refer :all])
  (:require [clojure.math.numeric-tower :refer :all])
)

(defn dominates
  [evaluate a b]
  (every? identity (map > (evaluate a) (evaluate b)))
)

(defn dominating
  [evaluate xs]
     (filter (fn [x] (not (some #(dominates evaluate %1 x) xs))) xs)
)

(defn add-to-archive
  [evaluate archive x size]
  
  (take size (dominating evaluate (concat [x] (filter #(not (= (evaluate %1) (evaluate x))) archive))))
)

(defn add-to-archive-if-spot
  [archive x size]
  (if (< (count archive) size)
    (distinct (concat [x] archive))
    archive))

(defn evolve
  [archive-size mutate ev p archive]
    (let [c (mutate p)]
      (trace "p" (ev p))
      (trace "kik" (ev c))
      (if (some identity (map #(dominates ev %1 c) archive))
        [p archive]
        (if (or (empty? archive) some identity (map #(dominates ev c %1) archive)) 
          (let [trimmed-archive (remove #(dominates ev c %1) archive)] 
              [c (add-to-archive ev trimmed-archive c archive-size)])
          [p archive]))))

        ;; (if (dominates ev c p)
          ;; [c (add-to-archive ev archive c archive-size)]
          ;; (if (some identity (map #(dominates ev c %1) archive))
            ;; (let [trimmed-archive (remove #(dominates ev c %1) archive)] 
              ;; [c (add-to-archive ev trimmed-archive c archive-size)])
            ;; [ (rand-nth [c p]) (add-to-archive-if-spot archive c archive-size)])))))

(defn simulate-evolution
  [it archive-size mutate evaluate initial-solution]
  (time 
   (let [acc (reduce 
                 (fn [x y] 
                   (trace "Second" (map evaluate (second x)))
                   (evolve archive-size mutate evaluate (first x) (second x))) 
                 [initial-solution []] 
                 (take it (repeat 0)))
         result (second acc)] 
     (dominating evaluate result))))
