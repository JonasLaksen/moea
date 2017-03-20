(ns evolution.imageprocessing
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [evolution.utils :refer :all])
  (:require [clojure.tools.trace :as d]
            [image-resizer.core :refer :all])
  (:require [clojure.core.matrix :refer :all])
)

(import 'java.io.File)
(import 'java.io.FileInputStream)
(import 'javax.imageio.ImageIO)
(import 'java.awt.image.BufferedImage)
(import 'java.awt.Color)



(defn integer->rgb
  "Takes a Java RGB-integer and returns a vector of RGB values\"
  \"Integer -> [0-255, 0-255, 0-255]"
  [i]
  (let [color (Color. i true)]
    [(.getRed color) (.getGreen color) (.getBlue color)]) 
)

(defn rgb->integer
  [rgb]
  (.getRGB (Color/decode (str "#" (str/join (map (fn [x] (format "%02x" x)) rgb)))))
)


(defn readimage
  "Takes a path and returns a 2d-list of the pixels of an image"
  [path]
  (let [imageBuffer (resize (ImageIO/read (FileInputStream. (File. path))) 100 100)]
    (matrix (partition (.getWidth imageBuffer) 
                       (for [y (range (.getHeight imageBuffer)) x (range (.getWidth imageBuffer))]
                                                 (integer->rgb (.getRGB imageBuffer x y))))) 
))

(defn writeimage
  "Takes a matrix and a filepath and writes an image"
  [image filepath]
  (let [imageBuffer (BufferedImage. (column-count image) (row-count image) BufferedImage/TYPE_INT_RGB)]
    (doseq [x (range (column-count image)) y (range (row-count image))]
      (.setRGB imageBuffer x y (mget image y x)))
    (ImageIO/write imageBuffer "png" (File. filepath))))

(defn draw-segments
  [filename segments image]
  (let [produced-image (reduce 
                        (fn 
                          [matrix segment] 
                          (let [color [(rand-int 256) (rand-int 256) (rand-int 256)]]
                            (set-indices 
                             matrix 
                             (map 
                              (fn [x] (get-matrix-position x matrix)) 
                              segment) 
                             (map rgb->integer (take (count segment) (repeat color))))))
                        image
                        segments)]
    (writeimage produced-image (str "results/" filename ".png")))
)
