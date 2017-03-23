(ns evolution.segmentation
  (:gen-class)
  (:require [clojure.core.matrix :refer [shape distance mget row-count column-count]]
            [loom.graph :refer :all]
            [loom.alg :refer :all]
            [taoensso.tufte :as tufte]
            [clojure.tools.trace :refer :all]
            [evolution.utils :refer :all]))

(defn chromosome->graph
  [chromosome] 
  (apply remove-edges (get chromosome :mst) (get chromosome :breaks)))

(defn mutate
  [x dimensions]
  (let [new-chromosome (if 
                           (and 
                            (< (rand) 0.5)
                            (> (count (get x :breaks)) (get x :min))) 
                         ;; Remove edge 
                         (assoc x :breaks (set (rest (shuffle (get x :breaks))))) 
                         (if 
                             (>= (count (get x :breaks)) (get x :max))
                           (assoc x :breaks (set (rest (shuffle (get x :breaks))))) 

                           ;; Add edge
                           (let [edges (take 10 (shuffle (edges (get x :mst))))
                                 best-edge (apply max-key #(weight (get x :mst) %) edges)]
                             (assoc x :breaks (set (conj (get x :breaks) best-edge)))) 
                           ))
        segments (connected-components (chromosome->graph new-chromosome))]
    (if (some #(< (count %) 10) segments) 
      (mutate x dimensions)
      (if (< (count (get new-chromosome :breaks)) (get x :min))
        (mutate new-chromosome dimensions)
        new-chromosome))))

(defn create-initial-solutions
  "Returns n solutions where one solution is a list of segments"
  [n image min-segments max-segments]
  (let [mst (time (prim-mst (apply weighted-graph 
                                   (mapcat 
                                    (fn [i] 
                                      (let [neighbors (neighbors i [(column-count image) (row-count image)])] 
                                        (map 
                                         (fn [neighbor] 
                                           [i neighbor (index-distance image i neighbor)]) 
                                         neighbors))) 
                                    (range (* (row-count image) (column-count image)))))))]
    ;; mst))
    (map 
     (fn [_] 
        (mutate {:min min-segments :max max-segments :breaks [] :mst mst } 
                [(column-count image) (row-count image)]))
     (range n))))

(defn crossover
  "Takes two solutions and returns a child"
  [a b]
  (let [mean (int (/ (+ (count (get a :breaks)) (count (get b :breaks))) 2))
        breaks (take mean (shuffle (set (concat (get a :breaks) (get b :breaks)))))]
    (assoc a :breaks breaks)
    ))


(defn edge-value
  [segments image] 
  (tufte/p ::edge-value
           (let [distance-memoized (memoize index-distance)
                 col-count (column-count image)
                 row-count (row-count image)
                 labeled-nodes (apply concat (map-indexed (fn [index segment] (map (fn [node] [node index ]) segment)) segments))
                 nodes->segment (into (sorted-map) labeled-nodes)
]
             (- (reduce + 
                        (pmap 
                         (fn [node] 
                           (reduce +
                                   (pmap
                                    (fn [neighbor]
                                      (let [neighbor-segment (get nodes->segment neighbor)]
                                        (if (= neighbor-segment (second node))
                                          0
                                          (apply distance-memoized image (sort [(first node) neighbor])))))
                                    (neighbors (first node) [col-count row-count]))))
                         labeled-nodes))))))
(defn connectivity-measure
  [segments image]
  (let [contains-memoized (memoize #(.contains %1 %2)) 
        labeled-nodes (apply concat (map-indexed (fn [index segment] (map (fn [node] [node index ]) segment)) segments)) 
        nodes->segment (into (sorted-map) labeled-nodes)]
    (float (reduce + 
                   (map 
                    (fn [labeled-node] (reduce + (map-indexed
                                                #(if (= (get nodes->segment %2) (second labeled-node)) 
                                                   0
                                                   (/ 1 (+ 1 %1)))
                                                (nearest-neighbors 1 (first labeled-node) image))))
                          labeled-nodes)
)))
)

(defn overall-deviation
  [segments image]
  (tufte/p ::overall-deviation
    (reduce + (map 
               (fn [segment] 
                 ;; (trace "segments" (count segment))
                 (let [c (centroid (map (fn [p] (pixel p image)) segment))]
                   (reduce + (map 
                              (fn 
                                [point] 
                                (distance c (pixel point image))) 
                              segment)))) 
               segments))))


(defn fitness 
  [solution image]
  (let [g (chromosome->graph solution)
        segments (connected-components g)]
    [ (- (edge-value segments image)) (- (overall-deviation segments image))]))

