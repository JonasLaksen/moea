(ns evolution.core
  (:gen-class)
  (:require [clojure.core.matrix :refer :all])
  (:require [evolution.segmentation :refer :all])
  (:require [evolution.evolution :refer :all])
  (:require [evolution.utils :refer :all])
  (:require [evolution.imageprocessing :refer :all]))

(def image (readimage "resources/images/test.png"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn lol
  [image]
  (let [best (nth (simulate-evolution 2 0.3 (fn [x] (fitness x image)) crossover (fn [x] (mutate x image)) (create-initial-solutions 20 image)) 0)]
    ;; best))
    (draw-segments (clusters best) image)))
