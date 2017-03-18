(ns evolution.utils
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.core.matrix :refer :all])
)

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
  (mget image (int (/ i (row-count image))) (mod i (column-count image)))
  )

(defn neighbors
  [i matrix]
  (let [init []
        left (if (= (mod i (column-count matrix)) 0)
               init
               (conj init (- i 1)))
        right (if (= (mod i (column-count matrix)) (- (column-count matrix) 1))
               left
               (conj left (+ i 1)))
        up (if (= (int (/ i (column-count matrix))) 0)
               right
               (conj right (- i (column-count matrix))))
        down (if (= (int (/ i (column-count matrix))) (-  (row-count matrix) 1))
               up
               (conj up (+ i (column-count matrix))))]
    (conj down i)))
