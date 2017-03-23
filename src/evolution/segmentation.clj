(ns evolution.segmentation
  (:gen-class)
  (:require [clojure.core.matrix :refer [shape distance mget row-count column-count]]
            [evolution.imageprocessing :refer :all]
            [loom.graph :refer :all]
            [loom.alg :refer :all]
            [taoensso.tufte :as tufte]
            [clojure.tools.trace :refer :all]
            [evolution.utils :refer :all]))

(import 'java.util.ArrayList)
(import 'fitness.Fitness)

(defn chromosome->graph
  [chromosome] 
  (apply remove-edges (get chromosome :mst) (get chromosome :breaks)))

(def arr-img (memoize (fn [image]
           (ArrayList. (map (fn [x] (ArrayList. (map (fn [y] (ArrayList. (map int y)) ) (ArrayList. x)))) image))             ))) 

(defn ev
  [segments image]
  (let [img (time (arr-img image))
        segs (ArrayList. (map #(ArrayList. (map int %)) segments))]
    (time (Fitness/edgeValue img segs))
)
)
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
                                 best-edge (apply max-key #(weight (get x :mst) %) edges)
                                 ;; random-node (rand-int (* (first dimensions) (second dimensions)))
                                 ;; random-neighbor (rand-nth (neighbors random-node dimensions))
                                 ]
                             (assoc x :breaks (set (conj (get x :breaks) best-edge)))) 
                           ))]
    (if (some #(< (count %) 10) (connected-components (chromosome->graph new-chromosome)))
      (mutate x dimensions)
      new-chromosome)))

(defn create-initial-solutions
  "Returns n solutions where one solution is a list of segments"
  [n image min-segments max-segments]
  (let [mst (prim-mst (apply weighted-graph 
                             (mapcat 
                              (fn [i] 
                                (let [neighbors (neighbors i [(column-count image) (row-count image)])] 
                                  (map 
                                   (fn [neighbor] 
                                     [i neighbor (index-distance image i neighbor)]) 
                                   neighbors))) 
                              (range (* (row-count image) (column-count image))))))]
    ;; mst))
    (map 
     (fn [_] 
       (reduce
        (fn [acc, _]
          (mutate acc [(column-count image) (row-count image)]))
        {:min min-segments :max max-segments :breaks [] :mst mst }
        (range max-segments))
       )
        ;; :breaks (set (take (+ min-segments (rand-int (- max-segments min-segments))) (shuffle (take 100 (sort-by #(weight mst %) (edges mst)))))) 
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
           (let [contains-memoized (memoize #(.contains %1 %2))
                 distance-memoized (memoize index-distance)
                 col-count (column-count image)
                 row-count (row-count image)]
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
                                               (neighbors x [col-count row-count]))))
                                    cluster)))
                         segments))))))

(defn connectivity-measure
  [segments image]
  (let [contains-memoized (memoize #(.contains %1 %2))]
    (float (reduce + (mapcat
                      (fn [segment] (map (fn [node] (reduce + (map-indexed
                                                         #(if (contains-memoized segment %2)
                                                            0
                                                            (/ 1 (+ 1 %1)))
                                                         (nearest-neighbors 1 node image))))
                                     segment))
                      segments))))
)

(defn overall-deviation
  [segments image]
  (tufte/p ::overall-deviation
    (reduce + (map 
               (fn [segment] 
                 (trace "segments" (count segment))
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
    [ (- (ev segments image)) (- (overall-deviation segments image))]))
