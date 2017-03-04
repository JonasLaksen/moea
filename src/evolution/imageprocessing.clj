(ns evolution.imageprocessing
  (:gen-class))

(import 'java.io.File)
(import 'java.io.FileInputStream)
(import 'javax.imageio.ImageIO)


(defn integer->rgb
  "Takes a Java RGB-integer and returns a vector of RGB values\"
  \"Integer -> [0-255, 0-255, 0-255]"
  [i]
  (let [hexas (Integer/toHexString i)]
    (->>  [(subs hexas 2 4) (subs hexas 4 6) (subs hexas 6 8)]
          (map (fn [x] (Integer/parseInt x 16)))
          (into []))) 
)


(defn readimage
  "Takes a path and returns a 2d-vector of the pixels of an image"
  [path]
  (let [imageBuffer (ImageIO/read (FileInputStream. (File. path)))]
    (count (for [y (range (.getHeight imageBuffer)) x (range (.getWidth imageBuffer))]
             (integer->rgb (.getRGB imageBuffer x y)))) 
))
