(ns evolution.core
  (:gen-class)
  (:require [clojure.core.matrix :refer :all]
            [taoensso.tufte :as tufte]
            [evolution.segmentation :refer :all]
            [loom.alg :refer [connected-components]]
            [evolution.evolution :refer :all]
            [evolution.utils :refer :all]
            [clojure.core.memoize :as memo]
            [evolution.imageprocessing :refer :all]))

(def image (readimage "resources/images/test.png"))
;; (def compile-time-min-level 2)
(tufte/add-basic-println-handler! {})

(defn lol
  [image]
  (tufte/profile {} (let [initial-solutions (create-initial-solutions 30 image)
                          ;; fit (memo/fifo #(fitness %1 image) {} :fifo/threshold 256)
                          fit #(fitness %1 image)
                          best (simulate-evolution 5 0.3 fit crossover #(mutate 0.001 %1 image) initial-solutions) ]
                      (draw-segments (connected-components best) image))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (lol image))

