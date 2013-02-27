(ns evok.mutation)

(def ^:private mutation-likelyhood 2)
(def ^:private command-instruction-likelyhood 5)
(def ^:private addition-to-modification-ratio 2)
(def ^:private deletion-to-replacement-ratio 3)
(def ^:private mutation-min-spacing 3)
(def ^:private mutation-max-spacing 10)

(defn rand-in-range [min max]
  (+ min (rand-int (inc (- max min)))))

(defn- random-instruction []
  (if (zero? (rand-int command-instruction-likelyhood))
    0
    (rand-int 100000)))

(defn- mutate-partition [part]
  (if (zero? (rand-int mutation-likelyhood))
    (let [instruction (random-instruction)]
      (if (zero? (rand-int addition-to-modification-ratio))
        ;; Addition
        (conj part instruction)
        ;; Modification
        (if (zero? (rand-int deletion-to-replacement-ratio))
          ;; Deletion
          (drop-last 1 part)
          ;; Replacement
          (conj (drop-last 1 part)
                instruction))))
    part))

(defn mutate-code [code]
  (let [mutation-spacing (rand-in-range mutation-min-spacing mutation-max-spacing)]
    (vec (mapcat mutate-partition
                 (partition mutation-spacing mutation-spacing [] code)))))

