(ns evok.util)

;; Return an integer N: 0 <= N < bound
(defn interpret-to-bound [x bound]
  (rem x bound))

(defn adjust-to-bounds [n min max]
  (if (< n min)
    min
    (if (> n max)
      max
      n)))