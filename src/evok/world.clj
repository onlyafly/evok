(ns evok.world)

(def time-points-per-tick 100)

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
                  (ref (Cell. nil nil)))
                (range *size*)))
        (range *size*)))

;;--------- Location
;; location = ref containing a Cell
;; coord = vector containing xy-coordinates

(defn location-by-coord [[x y :as _coord]]
  (-> @board-ref
      (nth y)
      (nth x)))

;;---------- Creatures & cagents
;; creature = record representing a creature
;; cagent = agent containing a CagentInfo record (representing the
;;          coordinate of a creature) 

(defrecord Creature [coord uid energy display pointer code stack direction generation])

(defrecord CagentInfo [uid coord])

(defn create-cagent [& {:keys [coord uid energy display pointer code stack direction generation]}]
  {:pre [coord uid]} ;;FIX this might cause errors
  (dosync
    (let [l (location-by-coord coord)
          c (Creature. coord uid energy display pointer code stack direction generation)]
      (alter l
             assoc :creature c)
      (let [cagent (agent (CagentInfo. uid coord))]
        ;;(set-error-mode! cagent :continue)
        (set-error-handler! cagent
                            (fn [a ex]
                              (prn :cagent-error ex)
                              ;;(prn :cause (.getCause ex))
                              ;;(prn :bean (bean ex))
                              ;;(prn :count-stackTrace (count (:stackTrace (bean ex))))
                              ;;(prn :count-suppressed (count (:suppressed (bean ex))))
                              ;;(prn (seq (.getStackTrace ex)))
                              (.printStackTrace ex)
                              ))        
        (comment (set-validator! cagent
                                 (fn [coord]
                                   (and (vector? coord)
                                        (= 2 (count coord))))))
        cagent))))

(defn genesis-code []
  [2 0 3 0 3 0 3 0 3 0 4 0])

(defn setup-cagents []
  (dosync
    ;; vector of cagents
    [(create-cagent :coord [0 0]
                    :uid (keyword (gensym "C0"))
                    :energy 100000
                    :display "O"
                    :pointer 0
                    :code (genesis-code)
                    :stack []
                    :direction 2
                    :generation 0)
     (create-cagent :coord [(dec *size*) (dec *size*)]
                    :uid (keyword (gensym "CX"))
                    :energy 100000
                    :display "X"
                    :pointer 0
                    :code (genesis-code)
                    :stack []
                    :direction 2
                    :generation 0)
     ]))

(comment
  )

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

(defn- stack-peek [creature]
  (if (pos? (count (:stack creature)))
    (peek (:stack creature))
    (rand-int 1000000)))

(defn- stack-pop [creature]
  (if (pos? (count (:stack creature)))
    (update-in creature [:stack] pop)
    creature))

(defn- increment-code-pointer [creature]
  (if (>= (inc (:pointer creature)) (count (:code creature)))
    (assoc creature :pointer 0)
    (update-in creature [:pointer] inc)))

(defn- update-creature-at-location [loc f]
  (alter loc update-in [:creature] f))

(defn- set-creature-at-location [loc c]
  (alter loc assoc :creature c))

;; Return an integer N: 0 <= N < bound
(defn- as-bounded-integer [x bound]
  (if (integer? x)
    (rem x bound)
    (rand-int bound)))

;;---------- Execution functions

(def instruction-table {0 :nop
                        1 :nop
                        2 :turn
                        3 :move
                        4 :procreate})

(def ^:private time-point-table {:move 100
                                 :turn 100
                                 :nop 1
                                 :procreate 100})

(def ^:private energy-point-table {:move 100
                                   :turn 100
                                   :nop 0
                                   ;; Procreate's energy should
                                   ;; based on the individual
                                   :procreate 0
                                   })

(defmulti exec (fn [_coord _loc _creature command]
                 (command-type command)))

(defmethod exec :move [coord loc creature _]
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
  (let [new-direction (as-bounded-integer (stack-peek creature) 4)]
    ;;FIX(prn :turn :coord coord :new-direction new-direction)
    (update-creature-at-location loc stack-pop)
    (when new-direction
      (update-creature-at-location loc
                                   #(assoc % :direction new-direction)))
    coord))

(defmethod exec :nop [coord _loc _creature _]
  coord)

(defmethod exec :procreate [coord loc creature _]
  (let [child-coord (delta-coord coord (:direction creature))
        child-loc (location-by-coord child-coord)]
    (when-not (:creature @child-loc)
      (let [child-energy (int (/ (:energy creature) 2))]
        
        ;; Only deduct energy when procreation is successful
        (update-creature-at-location loc #(update-in % [:energy] - child-energy))

        (alter cagents-ref conj (create-cagent :coord child-coord
                                               :uid (keyword (gensym (str "C" (:display creature))))
                                               :energy child-energy
                                               :display (:display creature)
                                               :pointer 0
                                               :code (:code creature)
                                               :stack []
                                               :direction (rand-int (count direction-delta-table))
                                               :generation (inc (:generation creature))))))
    coord))

;;---------- Cagent functions
;; Return the new coord for the creature after the action

(defmulti interpret (fn [_ _ val _]
                      {:pre [val]}
                      (if (zero? val) :zero :number)))
(defmethod interpret :zero [coord loc _val time-points]
  (let [command-int (as-bounded-integer (stack-peek (:creature @loc)) (count instruction-table))
        command (instruction-table command-int)
        command-time-points (time-point-table command)
        new-time-points (- time-points command-time-points)]
    ;;(prn :interpret-zero :command command :time time-points :newtime new-time-points)
    (if (>= new-time-points 0)
      (do
        ;; SIGNIFICANT ORDERING: instruction must be popped off prior
        ;; to execution
        (update-creature-at-location loc stack-pop) ; pop the instruction
        (let [creature (:creature @loc)
              new-coord (exec coord loc creature command)
              new-loc (location-by-coord new-coord)
              command-energy-points (energy-point-table command)]
          (update-creature-at-location new-loc #(update-in % [:energy] - command-energy-points))
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

(defn shake-creature [cagent-info coord loc creature time-points]
  {:pre [cagent-info coord loc creature time-points]}
  (let [pointer (:pointer creature)
        val (get-in creature [:code pointer])
        [new-coord new-time-points] (interpret coord loc val time-points)
        new-loc (location-by-coord new-coord)
        new-cagent-info (assoc cagent-info :coord new-coord)]
    (if (pos? (:energy (:creature @new-loc)))
      ;; Creature lives on
      [new-cagent-info new-time-points]
      ;; Creature dies
      (do
        ;;FIX(prn :death (:creature @new-loc))
        (set-creature-at-location new-loc nil)
        (comment (alter cagents-ref (fn [cagents]
                                      (remove (fn [cagent]
                                                (and (= new-coord (:coord @cagent))
                                                     (= (:uid creature) (:uid @cagent))))
                                              cagents))))
        [new-cagent-info 0]))))

(defn shake-cagent [^CagentInfo cagent-info time-points]
  {:pre [cagent-info time-points]}
  (dosync
    (let [{coord :coord} cagent-info
          loc (location-by-coord coord)
          creature (:creature @loc)]
      (shake-creature cagent-info coord loc creature time-points))))

(defn tick-cagent [^CagentInfo cagent-info time-points]
  (if (pos? time-points)
    ;; Time points remain in this tick
    (let [[new-cagent-info new-time-points] (shake-cagent cagent-info time-points)]
      (recur new-cagent-info
             new-time-points))
    ;; No time points remain
    cagent-info))

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
    (println (display-board @board-ref))))

;;---------- Tick

(defn tick []
  (try
    (dorun (map #(send-off % tick-cagent time-points-per-tick) @cagents-ref))
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
  (doseq [cagent @cagents-ref]
    (let [coord (:coord @cagent)
          creature (:creature @(location-by-coord coord))]
      (prn :coord coord :creature creature)))
  (doseq [cagent @cagents-ref]
    (let [coord (:coord @cagent)
          creature (:creature @(location-by-coord coord))
          err (agent-error cagent)]
      (when err
        (prn :coord coord)
        (prn :creature creature)
        (prn :err err)
        (.printStackTrace err))))
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
;; - kin recognition

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;