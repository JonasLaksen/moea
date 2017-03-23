(ns evolution.utils
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.core.matrix :refer :all])
)

(defn remove-once [pred coll]
  ((fn inner [coll]
     (lazy-seq
      (when-let [[x & xs] (seq coll)]
        (if (pred x)
          xs
          (cons x (inner xs))))))
   coll))

(defn mean
  [xs]
  (/ (apply + xs) (count xs)))

(defn get-matrix-position
  "Takes an index and returns [x y] coordinates for a matrix"
  [i matrix]
  [(int (/ i (column-count matrix))) (mod i (column-count matrix))]
  )

(defn index-distance 
  [matrix i j] 
  (distance (apply mget matrix (get-matrix-position i matrix)) (apply mget matrix (get-matrix-position j matrix)))
)

(defn centroid
  "Takes a list of vectors and returns an average vector"
  [vectors]
  (trace vectors)
  (vec (map mean (transpose vectors))))

(defn switch-2-random-nested-vector
  "[[1 2 3] [4 5 6]] -> [[1 4 3] [2 5 6]]"
  [vector]
  (let [a (rand-int (count vector))
        index-a (rand-int (count (get vector a)))
        elem-a (mget vector a index-a)
        b (rand-int (count vector))
        index-b (rand-int (count (get vector b)))
        elem-b (mget vector b index-b)
        first (assoc-in vector [a index-a] elem-b)
        result (assoc-in first [b index-b] elem-a)]
    result))

(defn pixel
  [i image]
  (do (trace i)
      (mget image (int (/ i (row-count image))) (mod i (column-count image))))
  )

(defn neighbors
  [i dimensions]
  (let [init []
        left (if (= (mod i (first dimensions)) 0)
               init
               (conj init (- i 1)))
        right (if (= (mod i (first dimensions)) (- (first dimensions) 1))
               left
               (conj left (+ i 1)))
        up (if (= (int (/ i (first dimensions))) 0)
               right
               (conj right (- i (first dimensions))))
        down (if (= (int (/ i (first dimensions))) (-  (second dimensions) 1))
               up
               (conj up (+ i (first dimensions))))]
    down))

(defn temp
  [levels i matrix] 
  (let [ns (neighbors i matrix)] 
    (if (= levels 0) 
      []
      (concat ns (mapcat #(temp (- levels 1) %1 matrix) ns)))))

(defn nearest-neighbor
  [i neighbor matrix]
  (nth (remove #(= %1 i) (distinct (temp 3 i matrix))) neighbor))

(defn nearest-neighbors
  [levels node matrix]
  (let [x (mod node (column-count matrix))
        y (int (/ node (column-count matrix)))
        pairs (remove #(= % [0 0]) (distinct (for [x (range 0 (+ 1 levels)) y (range 0 (+ 1  levels)) z [-1 1] w [-1 1]] [(* z x) (* w y)])))
        cut-east (filter 
                  #(< 
                    (first %) 
                    (- (column-count matrix) x)) pairs)
        cut-west (filter
                  #(>= (+ (first %) x) 0) cut-east)
        cut-north (filter
                   #(>= (+ (second %) y) 0) cut-west)
        cut-south (filter
                   #(> (row-count matrix) (+ y (second %))) cut-north)
]
    (map (fn [pair] (+ node (+ (first pair) (* (second pair) (column-count matrix))))) cut-south))
)
