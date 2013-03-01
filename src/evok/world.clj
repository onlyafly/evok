(ns evok.world
  (:require (evok [beagle :as beagle]
                  [mutation :as mutation]
                  [util :as util])))

(declare random-raw-value)

(def time-points-per-tick 100)
(def maximum-stack-size 1000)
(def trimmed-stack-size 500)

(def cagents-ref (ref nil))
(def board-ref (ref nil))

;;---------- Cells & board
;; cell = record representing a space on the board
;; board = 2D vector of locations

(defrecord Cell [food creature])

(def ^:dynamic *size* 20)

(defn setup-board []
  (mapv (fn [_]
          (mapv (fn [_]
                  (ref (Cell. 0 nil)))
                (range *size*)))
        (range *size*)))

;;--------- Location
;; location = ref containing a Cell
;; coord = vector containing xy-coordinates

(defn location-by-coord [[x y :as _coord]]
  (-> @board-ref
      (nth y)
      (nth x)))

;;---------- Creatures & Cinfos
;; creature = record representing a creature
;; cinfo = record representing the current coord of a creature
;; cagent = ref containing a cinfo

(defrecord Creature [uid energy display pointer code stack direction generation])

(defn display-creature-info [c]
  (let [{generation :generation
         direction :direction
         energy :energy
         uid :uid
         code :code} c]
    (prn :creature
         :uid uid
         :energy energy
         :direction direction
         :generation generation)
    ;;(prn :raw-code code)
    (prn :beagle-code (beagle/unbuild code))))

(defrecord Cinfo [uid coord])

(defn create-creature [& {:keys [uid energy display pointer code stack direction generation]}]
  (Creature. uid energy display pointer code stack direction generation))

(defn create-cagent [coord creature]
  (let [loc (location-by-coord coord)
        uid (:uid creature)]
    (alter loc
           assoc :creature creature)

    ;;FIX concurrent (MUST BE LAST)
    (comment
      (agent (Cinfo. uid coord)
             :error-handler (fn [a ex]
                              (prn :cagent-error ex)
                              ;;(prn :cause (.getCause ex))
                              ;;(prn :bean (bean ex))
                              ;;(prn :count-stackTrace (count (:stackTrace (bean ex))))
                              ;;(prn :count-suppressed (count (:suppressed (bean ex))))
                              ;;(prn (seq (.getStackTrace ex)))
                              (.printStackTrace ex))))
    
    ;;FIX non-concurrent (MUST BE LAST)
    (ref (Cinfo. uid coord))

    ))

(defn genesis-code []
  (beagle/build [:turn :turn :move :move :move :move :eat :procreate]))

(defn setup-cagents []
  {:post [(vector? %)]}
  (dosync
    [(create-cagent [0 0]
                    (create-creature 
                     :uid (keyword (gensym "CO_"))
                     :energy 100000
                     :display "O"
                     :pointer 0
                     :code (genesis-code)
                     :stack []
                     :direction 2
                     :generation 0))
     (create-cagent [(dec *size*) (dec *size*)]
                    (create-creature 
                     :uid (keyword (gensym "CX_"))
                     :energy 100000
                     :display "X"
                     :pointer 0
                     :code (genesis-code)
                     :stack []
                     :direction 2
                     :generation 0))
     ]))

;;---------- Instruction execution helpers

(defn- random-raw-value []
  (rand-int 100000))

(defn- adjust-to-bounds [n min max]
  (if (< n min)
    min
    (if (> n max)
      max
      n)))

(defn- instruction-type [v]
  (cond
   (keyword? v) v
   (number? v) :NUMBER
   (string? v) :STRING
   :else (do
           (prn :unknown-instruction-type v)
           :UNKNOWN)))

;; TODO maybe expand to 8 directions
(def direction-delta-table {0 [0 -1]  ; north
                            1 [1 0]   ; east
                            2 [0 1]   ; south
                            3 [-1 0]  ; west
                            4 [-1 -1] ; NW
                            5 [1 -1]  ; NE
                            6 [-1 1]  ; SW
                            7 [1 1]   ; SE
                            })

(defn delta-coord [[x y] direction]
  (let [[dx dy] (direction-delta-table direction)]
    [(adjust-to-bounds (+ x dx) 0 (dec *size*))
     (adjust-to-bounds (+ y dy) 0 (dec *size*))]))

(defn- stack-push [creature x]
  (let [c (if (> (count (:stack creature))
                 maximum-stack-size)
            (update-in creature [:stack] #(vec (take-last trimmed-stack-size %)))
            creature)]
    (update-in c [:stack]
               conj x)))

(defn- stack-peek [creature]
  (if (pos? (count (:stack creature)))
    (peek (:stack creature))
    (random-raw-value)))

(defn- stack-pop [creature]
  (if (pos? (count (:stack creature)))
    (update-in creature [:stack] pop)
    creature))

(defn- energy-value-of-code [code]
  {:pre [code (vector? code)]}
  (int (/ (count code) 2)))

(defn- energy-value-of-creature [^Creature c]
  (+ (:energy c)
     (energy-value-of-code (:code c))))

(defn- increment-code-pointer [creature]
  (if (>= (inc (:pointer creature)) (count (:code creature)))
    (assoc creature :pointer 0)
    (update-in creature [:pointer] inc)))

(defn- update-creature-at-location [loc fn]
  (alter loc update-in [:creature] fn))

(defn- update-food-at-location [loc fn]
  (alter loc update-in [:food] fn))

(defn- set-creature-at-location [loc c]
  (alter loc assoc :creature c))

(defn- add-food-to-location [loc food]
  (alter loc update-in [:food] + food))

;;---------- Instruction execution

(defmulti exec (fn [_coord _loc _creature instruction]
                 (instruction-type instruction)))

(defmethod exec :move [coord loc creature _]
  {:post [(vector? %)]}
  (let [new-coord (delta-coord coord (:direction creature))
        new-loc (location-by-coord new-coord)]
    ;;FIX(prn :move :coord coord)

    (if (:creature @new-loc)
      ;; The space ahead is occupied
      coord
      ;; The space is unoccupied
      (do
        ;; SIGNIFICANT ORDERING: in case of moving to and from same coord
        (set-creature-at-location loc nil)
        (set-creature-at-location new-loc creature)
        new-coord))))

(defmethod exec :turn [coord loc creature _]
  {:post [(vector? %)]}
  (let [new-direction (util/as-bounded-integer (stack-peek creature) (count direction-delta-table))]
    ;;FIX(prn :turn :coord coord :new-direction new-direction)
    (update-creature-at-location loc stack-pop)
    (when new-direction
      (update-creature-at-location loc
                                   #(assoc % :direction new-direction)))
    coord))

(defmethod exec :nop [coord _loc _creature _]
  {:post [(vector? %)]}
  coord)

(defmethod exec :procreate [coord loc creature _]
  {:post [(vector? %)]}
  (let [child-coord (delta-coord coord (:direction creature))
        child-loc (location-by-coord child-coord)]
    (when-not (:creature @child-loc)
      (let [child-gift-energy (int (/ (:energy creature) 2))
            child (create-creature :uid (keyword (gensym (str "C" (:display creature))))
                                   :energy child-gift-energy
                                   :display (:display creature)
                                   :pointer 0
                                   :code (mutation/mutate-code (:code creature))
                                   :stack []
                                   :direction (rand-int (count direction-delta-table))
                                   :generation (inc (:generation creature)))
            child-total-energy (energy-value-of-creature child)]
        
        ;; Only deduct energy when procreation is successful
        (update-creature-at-location loc #(update-in % [:energy] - child-total-energy))

        (alter cagents-ref conj (create-cagent child-coord child))))
    coord))

(defmethod exec :eat [coord loc creature _]
  {:post [(vector? %)]}
  (when (pos? (:food @loc))
    ;;(prn :eat coord :food (:food @loc) :creature-uid (:uid creature) :creature-energy (:energy creature))
    (update-food-at-location loc (fn [_] 0))
    (update-creature-at-location loc
                                 #(update-in % [:energy] + (:food @loc))))
  coord)

(defmethod exec :startblock [coord loc creature _]
  {:post [(vector? %)]}
  ;; TODO
  coord)

(defmethod exec :endblock [coord loc creature _]
  {:post [(vector? %)]}
  ;; TODO
  coord)

(defmethod exec :if [coord loc creature _]
  {:post [(vector? %)]}
  ;; TODO
  coord)

(defmethod exec :display [coord loc creature _]
  {:post [(vector? %)]}
  ;; TODO
  coord)

;;---------- Cinfo functions

(defmulti interpret (fn [_ _ val _]
                      {:pre [val]}
                      (if (zero? val) :zero :number)))
(defmethod interpret :zero [coord loc _val time-points]
  (let [instruction (beagle/raw-value->instruction (stack-peek (:creature @loc)))
        instruction-time-points (beagle/time-point-table instruction)
        new-time-points (- time-points instruction-time-points)]
    ;;(prn :interpret-zero :instruction instruction :time time-points :newtime new-time-points)
    (if (>= new-time-points 0)
      (do
        ;; SIGNIFICANT ORDERING: raw value must be popped off prior
        ;; to execution
        (update-creature-at-location loc stack-pop) ; pop the raw-value
        (let [creature (:creature @loc)
              new-coord (exec coord loc creature instruction)
              new-loc (location-by-coord new-coord)
              instruction-energy-points (beagle/energy-point-table instruction)]
          (update-creature-at-location new-loc #(update-in % [:energy] - instruction-energy-points))
          (update-creature-at-location new-loc increment-code-pointer)
          [new-coord new-time-points]))
      [coord 0])))
(defmethod interpret :number [coord loc number time-points]
  (let [new-time-points (dec time-points)]
    ;;(prn :interpret-number :number number :time time-points)
    (update-creature-at-location loc
                                 #(stack-push % number))
    (update-creature-at-location loc increment-code-pointer)
    [coord new-time-points]))

(defn shake-creature [cinfo coord loc creature time-points]
  {:pre [cinfo coord loc creature time-points]}
  (let [pointer (:pointer creature)
        val (get-in creature [:code pointer])
        [new-coord new-time-points] (interpret coord loc val time-points)
        new-loc (location-by-coord new-coord)
        new-cinfo (assoc cinfo :coord new-coord)]
    (if (pos? (:energy (:creature @new-loc)))
      ;; Creature lives on
      [new-cinfo new-time-points]
      ;; Creature dies
      (do
        ;;FIX(prn :death (:creature @new-loc))
        (set-creature-at-location new-loc nil)
        (add-food-to-location new-loc (energy-value-of-creature creature))
        ;; Return a nil as the cinfo, marking it for deletion
        [nil 0]))))

(defn shake-cinfo [^Cinfo cinfo time-points]
  {:pre [cinfo time-points]}
  (dosync
    (let [{coord :coord} cinfo
          loc (location-by-coord coord)
          creature (:creature @loc)]
      ;;(prn :shake-cinfo cinfo coord loc creature)
      (shake-creature cinfo coord loc creature time-points))))

(defn tick-cinfo [^Cinfo cinfo time-points]
  (if (and (pos? time-points)
           cinfo)
    ;; Time points remain in this tick
    (let [[new-cinfo new-time-points] (shake-cinfo cinfo time-points)]
      (recur new-cinfo
             new-time-points))
    ;; No time points remain
    cinfo))

;;---------- Display

(defn display-location [loc]
  (let [cell @loc]
    (if-let [creature (:creature cell)]
      (:display creature)
      (if (pos? (:food cell))
        "#"
        "-"))))

(defn display-row [row]
  (apply str (map display-location row)))

(defn display-board [board]
  (clojure.string/join "\n" (map display-row board)))

(defn show! []
  (io!
    (println (display-board @board-ref))))

;;---------- Tick

(defn tick []
  (try
    ;; Update the cagents with the new cinfos
    
    ;; FIX non-concurrent
    (doseq [cagent @cagents-ref]
      (let [cinfo @cagent
            new-cinfo (tick-cinfo cinfo time-points-per-tick)]
        (dosync
          (ref-set cagent new-cinfo))))
    
    ;; FIX concurrent
    (comment
      (dorun (map #(send-off % tick-cinfo time-points-per-tick) @cagents-ref))
      (dorun (map await @cagents-ref)))
    
    ;; Prune dead creatures (dead = cagent's value is nil)
    (dosync
      (alter cagents-ref (fn [cagents]
                           (remove #(nil? @%) cagents))))
    (catch Exception e
      (.printStackTrace e))))

(defn tick-times [times]
  (dorun (map (fn [_]
                (tick))
              (range times))))

;;---------- Init

(defn init []
  (dosync
    (ref-set board-ref (setup-board))
    (ref-set cagents-ref (setup-cagents))))

(comment
  (do
    (load-file "/home/kevin/code/evok/src/evok/world.clj")
    (use 'evok.world)
    (init))
  (do (show!) (println))
  (do (tick) (show!) (println "DONE"))
  (do (tick-times 100) (show!) (println))
  (deref cagents-ref)
  (deref board-ref)
  (do (tick-times 100) (show!) (println))
  (doseq [cagent @cagents-ref]
    (let [coord (:coord @cagent)
          creature (:creature @(location-by-coord coord))]
      (prn :coord coord :creature creature)))

  (do
    (tick-times 10)
    (show!)
    (println)
    (doseq [cagent @cagents-ref]
    (let [coord (:coord @cagent)
          creature (:creature @(location-by-coord coord))]
      (prn :coord coord :creature creature))))
  
  (deref (location-by-coord [0 1]))

  (do
    (defn bar [n]
      (/ n 0))
    (defn foo [n]
      (bar n))
    (let [a (agent 0 :error-handler (fn [ag ex]
                                      (prn :error ex)
                                      (.printStackTrace ex)))]
      (send a foo)
      (Thread/sleep 100)
      (prn :result @a)))
  (do
    (let [a (agent 0)]
      (send a inc)
      (Thread/sleep 100)
      (prn :result @a)))
  
  )


;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; example instructions                                                     ;;
;; - reproduce (takes a value which is the amount of energy newborn         ;;
;; starts with, which is deducted from parent)                              ;;
;;    - the amount of instructions that must be copied to the child also    ;;
;; consumes some enrgy from parent                                          ;;
;; - gather-food from neighbor (marks the eatee as being eaten with a flag) ;;
;;  - hopefully evolution will provide a defense against being eaten        ;;
;; - check-status-of-flag
;; - how much energy left?
;; - kin recognition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;