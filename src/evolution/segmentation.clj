(ns evolution.segmentation
  (:gen-class)
  (:require [clojure.core.matrix :refer [distance mget row-count column-count]]
            [evolution.imageprocessing :refer :all]
            [loom.graph :refer :all]
            [loom.alg :refer :all]
            [taoensso.tufte :as tufte]
            [clojure.tools.trace :refer :all]
            [evolution.utils :refer :all]))


(defn mutate
  [times solution image]
  (tufte/p ::mutate 
           (prim-mst (reduce
                      (fn [solution x]
                        (let [node (rand-int (count (nodes solution)))
                              neighbor (rand-nth (neighbors node image)) 
                              removed (remove-edges solution [node (rand-nth (successors solution node))]) 
                              added (add-edges removed [node neighbor (index-distance image node neighbor)])] 
                          added))
                      solution
                      (range times)))))

(defn create-initial-solutions
  "Returns n solutions where one solution is a list of segments"
  [n image]
  (let [mst (prim-mst (apply weighted-graph 
                             (mapcat 
                              (fn [i] 
                                (let [neighbors (neighbors i image)] 
                                  (map 
                                   (fn [neighbor] 
                                     [i neighbor (index-distance image i neighbor)]) 
                                   neighbors))) 
                              (range (* (row-count image) (column-count image))))))]
    (map
     #(mutate 1000 %1 image)
     (take n (repeat mst)))))

(defn crossover
  "Takes two solutions and returns a child"
  [a b]
  (tufte/p ::crossover 
           (prim-mst 
            (apply weighted-graph 
                   (concat 
                    (map (fn [x] {x {}}) (nodes a)) 
                    (mapcat (fn [node] 
                              (let [chosen (rand-nth [a b])] 
                                (map 
                                 (fn [succ] 
                                   [node succ (weight chosen [node succ])]) 
                                 (successors chosen node)))) 
                            (nodes a)))))))

(defn edge-value
  [solution image] 
  (tufte/p ::edge-value
           (let [contains-memoized (memoize #(.contains %1 %2))
                 distance-memoized (memoize index-distance)]
             (- (reduce +
                        (map 
                         (fn [cluster]
                           (reduce +
                                   (map
                                    (fn [x] 
                                      (reduce +
                                              (map
                                               (fn [neighbor]
                                                 (if (contains-memoized cluster neighbor)
                                                   0
                                                   (apply distance-memoized image (sort [x neighbor]))))
                                               (neighbors x image))))
                                    cluster)))
                         (connected-components solution)))))))

(defn connectivity-measure
  [solution image]
  (reduce + (mapcat
             (fn [cluster]
               (map
                (fn [node]
                  (reduce + (map
                             #(let [neighbor (nearest-neighbor node %1 image)]
                                (if (.contains cluster neighbor)
                                  0
                                  (/ 1 %1)))
                             (range 1 8))))
                cluster))
             (connected-components solution)))
)

(defn overall-deviation
  [solution image]
  (tufte/p ::overall-deviation
    (reduce + (map 
               (fn 
                 [segment] 
                 (let [c (centroid (map (fn [p] (pixel p image)) segment))]
                   (reduce + (map 
                              (fn 
                                [point] 
                                (distance c (pixel point image))) 
                              segment)))) 
               (connected-components solution)))))

(defn fitness 
  [solution image]
    [(edge-value solution image) (overall-deviation solution image) (connectivity-measure solution image)])

