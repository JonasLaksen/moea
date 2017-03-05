(ns evolution.utils
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

(defn euclidean
  "Takes two n-dimensional points and returns the distance between these"
  [p1 p2]
  (let [pairs (map vector p1 p2)]
    (math/sqrt (reduce + (map (fn [x] (math/expt (reduce - x) 2)) pairs)))))
