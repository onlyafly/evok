(ns evok.beagle
  (:require clojure.set))

(declare build-item)

(def command-marker 0)

;;                 Instruction      TimePoints     EnergyPoints
(def instructions [[:nop            1              0]  ;zero is also the command instruction
                   [:startblock     1              1]
                   [:endblock       1              1]
                   [:if             1              1]
                   [:display        100            100]
                   [:turn           100            100]
                   [:move           100            100]
                   [:procreate      0              100] ; energy cost calculated dynamically
                   [:eat            100            100]
                   ])

(def energy-point-table (into {} (for [i (range (count instructions))]
                                   (let [v (nth instructions i)]
                                     [(nth v 0) ; name
                                      (nth v 2) ; energy points
                                      ]))))
(def time-point-table (into {} (for [i (range (count instructions))]
                                 (let [v (nth instructions i)]
                                   [(nth v 0) ; name
                                    (nth v 1) ; time points
                                    ]))))
(def instruction-table (into {} (for [i (range (count instructions))]
                                  (let [v (nth instructions i)]
                                    [i         ; instruction code
                                     (nth v 0) ; name
                                     ]))))

(def name-table (clojure.set/map-invert instruction-table))

(defmulti build-item-method (fn [x] (cond (vector? x) :VECTOR
                                         (integer? x) :INTEGER
                                         (keyword? x) :KEYWORD
                                         :else :UNKNOWN)))
(defmethod build-item-method :KEYWORD [instruction-name]
  [(name-table instruction-name) command-marker])
(defmethod build-item-method :INTEGER [n]
  [n])
(defmethod build-item-method :VECTOR [item]
  (vec (concat (build-item :startblock)
               (mapcat build-item item)
               (build-item :endblock))))

(defn build-item [item]
  {:post [(vector? %)]}
  (build-item-method item))

(defn build [code]
  (vec (mapcat build-item code)))