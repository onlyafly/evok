(ns evok.creature)

(defn genesis-code []
  [:move-random])

(defn genesis []
  {:energy 100000
   :pos [0 0]
   :display "g"
   :pointer 0
   :code (genesis-code)
   :stack []
   :alive true})

(defn display-creature [c]
  (if c
    (:display c)
    "-"))