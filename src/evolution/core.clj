(ns evolution.core
  (:gen-class)
  (:require [clojure.core.matrix :refer :all]
            [taoensso.tufte :as tufte]
            [evolution.segmentation :refer :all]
            [loom.alg :refer [dijkstra-span bf-traverse connected-components prim-mst]]
            [loom.graph :refer [weight nodes weighted-graph weighted-digraph]]
            [evolution.evolution :refer :all]
            [evolution.utils :refer :all]
            [clojure.core.memoize :as memo]
            [evolution.imageprocessing :refer :all]))

(def image (readimage "resources/images/1/test.jpg"))
;; (tufte/add-basic-println-handler! {})

(defn lol
  [image]
  (tufte/profile {:level 0} (let [initial-solution (first (create-initial-solutions 1 image))
                          fit (memo/fifo #(fitness %1 image) {} :fifo/threshold 256)
                          ;; fit #(fitness %1 image)
                          result (simulate-evolution 100 10 #(mutate 1 %1 image) fit initial-solution) ]
                      (doall (map-indexed #(draw-segments %1 (connected-components %2) image) (take 5 result))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (tufte/add-basic-println-handler! {})
  (println "Hello, World!")
  (lol image))

