(ns evok.world)

(def time-points-per-tick 100)

;;---------- Cells & board
;; cell = record representing a space on the board
;; board = 2D vector of locations

(defrecord Cell [food creature])

(def ^:dynamic *size* 40)

(def board
  (mapv (fn [_]
          (mapv (fn [_]
                  (ref (Cell. nil nil)))
                (range *size*)))
        (range *size*)))

;;--------- Location
;; location = ref containing a Cell
;; coord = vector containing xy-coordinates

(defn location-by-coord [[x y :as _coord]]
  (-> board
      (nth x)
      (nth y)))

;;---------- Creatures & cagents
;; creature = record representing a creature
;; cagent = agent containing a coord (representing the coordinate of a creature)

(defrecord Creature [energy display pointer code stack direction])

(defn create-cagent [coord & {:keys [energy display pointer code stack direction]}]
  (dosync
    (let [l (location-by-coord coord)
          c (Creature. energy display pointer code stack direction)]
      (alter l
             assoc :creature c)
      (agent coord))))

;;--------- Setup

(defn genesis-code []
  [:turn :move])

(defn setup-cagents []
  (dosync
    ;; vector of cagents
    [(create-cagent [0 0]
                    :energy 100000
                    :display "O"
                    :pointer 0
                    :code (genesis-code)
                    :stack []
                    :direction 2)
     (create-cagent [(dec *size*) (dec *size*)]
                    :energy 100000
                    :display "X"
                    :pointer 0
                    :code (genesis-code)
                    :stack []
                    :direction 2)]))

(def cagents-ref (ref (setup-cagents)))

;;---------- Cagent helpers

(defn- adjust-to-bounds [n min max]
  (if (< n min)
    min
    (if (> n max)
      max
      n)))

(defn- command-type [command]
  (cond
   (keyword? command) command
   (number? command) :NUMBER
   (string? command) :STRING
   :else (do
           (prn :unknown-command-type command)
           :UNKNOWN)))

(def ^:private time-point-table {:NUMBER 1
                                 :STRING 1
                                 :UNKNOWN 1
                                 :move 100
                                 :turn 100})

(def ^:private energy-point-table {:NUMBER 0
                                   :STRING 0
                                   :UNKNOWN 0
                                   :move 100
                                   :turn 100})

;; TODO maybe expand to 8 directions
(def direction-delta-table {0 [0 -1] ; north
                            1 [1 0]  ; east
                            2 [0 1]  ; south
                            3 [-1 0] ; west
                            })

(defn delta-coord [[x y] direction]
  (let [[dx dy] (direction-delta-table direction)]
    [(adjust-to-bounds (+ x dx) 0 (dec *size*))
     (adjust-to-bounds (+ y dy) 0 (dec *size*))]))

;;---------- Command interpreter helpers

(defn- stack-push [creature x]
  (update-in creature [:stack]
             conj x))

(defn- stack-pop [creature]
  (when (pos? (count (:stack creature)))
    (pop (:stack creature))))

(defn- increment-code-pointer [creature]
  (if (>= (inc (:pointer creature)) (count (:code creature)))
    (assoc creature :pointer 0)
    (update-in creature [:pointer] inc)))

(defn- update-creature [loc f]
  (alter loc update-in [:creature] f))

(defn- set-creature [loc c]
  (alter loc assoc :creature c))

;; Return an integer N: 0 <= N < bound
(defn- as-bounded-integer [x bound]
  (if (integer? x)
    (rem x bound)
    (rand-int bound)))

;;---------- Cagent functions
;; Return the new coord for the creature after the action

(defmulti exec (fn [coord _loc command]
                 (command-type command)))

(defmethod exec :NUMBER [coord loc number]
  (update-creature loc
                   #(stack-push % number))
  coord)

(defmethod exec :move [coord loc _]
  (let [creature (:creature @loc)
        new-coord (delta-coord coord (:direction creature))
        new-loc (location-by-coord new-coord)]

    (if (:creature @new-loc)
      ;; The space ahead is occupied
      coord
      ;; The space is unoccupied
      (do
        ;; SIGNIFICANT ORDERING: in case of moving to and from same coord
        (set-creature loc nil)
        (set-creature new-loc creature)
        new-coord))))

(defmethod exec :turn [coord loc _]
  (let [creature (:creature @loc)
        new-direction (as-bounded-integer (stack-pop creature) 4)]
    (when new-direction
      (update-creature loc
                       #(assoc % :direction new-direction)))
    coord))

(defn shake-cagent [coord time-points]
  (dosync
    (let [loc (location-by-coord coord)
          cell @loc
          c (:creature cell)
          pointer (:pointer c)
          command (get-in c [:code pointer])
          type (command-type command)
          command-time-points (time-point-table type)
          command-energy-points (energy-point-table type)]
      (comment
        (prn :shake-----------------------)
        (prn :coord coord)
        (prn :cell @loc))
      (if (<= command-time-points time-points)
        ;; Time points enough for command
        (let [new-coord (exec coord loc command)
              new-loc (location-by-coord new-coord)
              new-time-points (- time-points command-time-points)]
          (update-creature new-loc increment-code-pointer)
          (alter new-loc update-in [:creature :energy]
                 - command-energy-points)
          ;;TODO update-life-status
          [new-coord new-time-points])
        ;; Out of time points
        [coord 0]))))

(defn tick-cagent [coord time-points]
  (if (pos? time-points)
    ;; Time points remain in this tick
    (let [[new-coord new-time-points] (shake-cagent coord time-points)]
      (recur new-coord
             new-time-points))
    ;; No time points remain
    coord))

;;---------- Display

(defn display-location [loc]
  (let [cell @loc]
    (if-let [creature (:creature cell)]
      (:display creature)
      "-")))

(defn display-row [row]
  (apply str (map display-location row)))

(defn display-board [board]
  (clojure.string/join "\n" (map display-row board)))

(defn show! []
  (io!
    (println (display-board board))))

;;---------- Tick

(defn tick []
  (dorun (map #(send-off % tick-cagent time-points-per-tick) @cagents-ref)))

(defn tick-times [times]
  (dorun (map (fn [_]
                (tick))
              (range times))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time points each tick:                                                   ;;
;; - each creature has X number of time points per tick. Each instruction   ;;
;; takes Y number of time points to execute                                 ;;
;; - actions in the physical world take at least one tick to complete       ;;
;; - instructions that are about flow control, pushing data on the stack,   ;;
;; etc, are very inexpensive in time                                        ;;
;;                                                                          ;;
;; energy consumed by instructions:                                         ;;
;; - each instruction has some cost in energy                               ;;
;;  - physical instructions are expensive                                   ;;
;;  - mental instructions are cheap                                         ;;
;; - when a creature's energy runs out, it dies                             ;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;