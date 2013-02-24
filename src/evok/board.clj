(ns evok.board
  (:require (evok [creature :as creature])))

(def ^:dynamic *size* [1 1])

(defn- new-board [width height]
  (vec (for [i (range width)]
         (vec (for [j (range height)]
                "-")))))

(defn build-board [creatures]
  (let [[width height] *size*]
    (reduce (fn [board creature]
              (let [[x y] (:pos creature)]
                (assoc-in board [x y] (creature/display-creature creature))))
            (new-board width height)
            creatures)))

(defn- display-row [row]
  (apply str row))

(defn display-board [creatures]
  (let [board (build-board creatures)]
    (clojure.string/join "\n" (for [row board]
                                (display-row row)))))