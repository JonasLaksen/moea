(ns evolution.core
  (:gen-class)
  (:require 
            [taoensso.tufte :as tufte]
            [clojure.core.matrix :refer [shape distance mget row-count column-count]]
            [evolution.segmentation :refer :all]
            [loom.alg :refer [connected-components prim-mst]]
            [loom.graph :refer [weight nodes weighted-graph weighted-digraph]]
            [evolution.evolution :refer :all]
            [evolution.utils :refer :all]
            [clojure.core.memoize :as memo]
            [evolution.imageprocessing :refer :all]))

(def image (readimage "resources/images/1/test.jpg"))
;; (def image (readimage "carlton.jpg"))
(tufte/add-basic-println-handler! {})

(defn lol
  [image]
  (tufte/profile {:level 0} (let [initial-solutions (create-initial-solutions 5 image 5 15)
                          fit (memo/fifo #(fitness %1 image) {} :fifo/threshold 256)
                          result (simulate-evolution 10 fit crossover #(mutate %1 [(column-count image) (row-count image)]) initial-solutions) ]
                              (doall (map-indexed #(draw-segments %1 (connected-components (chromosome->graph %2)) image) result)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (tufte/add-basic-println-handler! {})
  (println "Hello, World!")
  (lol image))

