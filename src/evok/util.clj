(ns evok.util)

;; Return an integer N: 0 <= N < bound
(defn as-bounded-integer [x bound]
  (if (integer? x)
    (rem x bound)
    (rand-int bound)))