(ns evolution.segmentation
  (:gen-class)
  (:require [clojure.core.matrix :refer [distance mget row-count column-count]])
  (:require [evolution.imageprocessing :refer :all])
  (:require [loom.graph :refer [graph]])
  (:require [loom.alg :refer :all])
  (:require [clojure.tools.trace :refer :all])
  (:require [evolution.utils :refer :all]))

(defn create-initial-solutions
  "Returns n solutions where one solution is a list of segments"
  [n image]
  (for [x (range n)] 
    ;; (shuffle (range (* (row-count image) (column-count image))))))
    (map (fn [i] (rand-nth (neighbors i image))) (range (* (row-count image) (column-count image))))))

(defn clusters
  "Takes a solution (list of pixel position (int)) and returns a list of clusters"
  [solution]
  (connected-components 
   (apply graph 
          (map vector solution (range (count solution))))))

(defn mutate
  [solution image]
  (map-indexed (fn [i e] (if (= (rand-int 10) 0) 
                           (rand-nth (neighbors i image))
                           e)
                 ) (range (count solution))))

(defn crossover
  "Takes two solutions and returns a child"
  [a b]
  (trace "Parent A" (count (clusters a)))
  (trace "Parent B" (count (clusters b)))
  (let [index (rand-int (count a))
        child (concat (take index a) (drop index b))]
    child))
  ;; (map (fn [x y] (rand-nth [x y])) a b))

(defn overall-deviation
  [solution image]
  (let [segments (clusters solution)]
    (reduce + (map 
               (fn 
                 [segment] 
                 (let [c (centroid (map (fn [p] (pixel p image)) segment))]
                   (reduce + (map 
                              (fn 
                                [point] 
                                (distance c (pixel point image))) 
                              segment)))) 
               segments))))

(defn fitness 
  [solution image]
  (let [od (overall-deviation solution image)]
    ;; (trace "Fitness" (- od))
    (- od)))

