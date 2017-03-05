(ns evolution.imageprocessing
  (:gen-class)
  (:require [clojure.string :as str])
  (:require
    [taoensso.timbre :as timbre
      :refer [log  trace  debug  info  warn  error  fatal  report
              logf tracef debugf infof warnf errorf fatalf reportf
              spy get-env]])
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
  (let [imageBuffer (ImageIO/read (FileInputStream. (File. path)))]
    (partition (.getWidth imageBuffer) (for [y (range (.getHeight imageBuffer)) x (range (.getWidth imageBuffer))]
                                         (integer->rgb (.getRGB imageBuffer x y)))) 
))

(defn writeimage
  "Takes a 2d-list and a filepath and writes an image"
  [image filepath]
  (let [imageBuffer (BufferedImage. (count (nth image 0)) (count image) BufferedImage/TYPE_INT_RGB)]
    (doseq [y (range (count image)) x (range (count (nth image 0)))]
      (.setRGB imageBuffer x y (rgb->integer (nth (nth image y) x))))
    (ImageIO/write imageBuffer "jpg" (File. filepath))))


(def image (readimage "resources/images/1/test.jpg"))
