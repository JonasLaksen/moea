(ns evolution.core
  (:gen-class)
  (:require 
            [taoensso.tufte :as tufte]
            [evolution.segmentation :refer :all]
            [loom.alg :refer [connected-components prim-mst]]
            [loom.graph :refer [weight nodes weighted-graph weighted-digraph]]
            [evolution.evolution :refer :all]
            [evolution.utils :refer :all]
            [clojure.core.memoize :as memo]
            [evolution.imageprocessing :refer :all]))

(def image (readimage "resources/images/1/test.jpg"))
(tufte/add-basic-println-handler! {})

(defn lol
  [image]
  (tufte/profile {:level 0} (let [initial-solutions (create-initial-solutions 10 image)
                          fit (memo/fifo #(fitness %1 image) {} :fifo/threshold 256)
                          ;; fit #(fitness %1 image)
                          result (simulate-evolution 20 fit crossover #(mutate 5 %1 image) initial-solutions) ]
                      (doall (map-indexed #(draw-segments %1 (connected-components %2) image) (take 5 result))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (tufte/add-basic-println-handler! {})
  (println "Hello, World!")
  (lol image))

