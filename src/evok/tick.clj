(ns evok.tick
  (:require (evok [board :as board]
                  [creature :as creature])))

;;---------- Helper functions

(defn- spush [c x]
  (update-in c [:stack]
             conj x))

(defn- cinc [c]
  (if (>= (inc (:pointer c)) (count (:code c)))
    (assoc c :pointer 0)
    (update-in c [:pointer] inc)))

(defn- command-type [command]
  (cond
   (keyword? command) command
   (number? command) :NUMBER
   (string? command) :STRING
   :else :UNKNOWN))

;; 100 time-points per turn
(def ^:private time-point-table {:NUMBER 1
                                 :STRING 1
                                 :UNKNOWN 1
                                 :move-random 100})

(def ^:private energy-table {:NUMBER 0
                             :STRING 0
                             :UNKNOWN 0
                             :move-random 100})

(defn- adjust-int-to-bounds [n min max]
  (if (< n min)
    min
    (if (> n max)
      max
      n)))

(defn- move-pos-to-board [[x y :as p]]
  (let [[w h] board/*size*]
    [(adjust-int-to-bounds x 0 (dec w))
     (adjust-int-to-bounds y 0 (dec h))]))

(defn- update-life-status [c]
  (if (not (pos? (:energy c)))
    (assoc c
      :alive false
      :display "*")
    c))

;;---------- Interpreter of commands

(defmulti exec (fn [_ command] (command-type command)))

(defmethod exec :NUMBER [c number]
  (-> (spush c number)))

(defmethod exec :move-random [c _]
  (let [dimension (rand-int 2)
        polarity (first (shuffle [inc dec]))
        new-pos (update-in (:pos c) [dimension] polarity)
        fixed-pos (move-pos-to-board new-pos)]
    #_(prn :exec :move-random)
    (-> c
        (assoc :pos fixed-pos))))

(defn exec-next [c remaining-time-points]
  (let [pointer (:pointer c)
        command (get-in c [:code pointer])
        type (command-type command)
        command-time-points (get time-point-table type)
        command-energy-points (get energy-table type)]
    #_(prn :exec-next pointer command type command-time-points remaining-time-points)
    (if (<= command-time-points remaining-time-points)
      (let [new-c (-> (exec c command)
                      cinc
                      (update-in [:energy] - command-energy-points)
                      update-life-status)
            new-remaining-time-points (- remaining-time-points command-time-points)]
        [new-c new-remaining-time-points])
      [c 0])))

;;---------- Tick

(defn tick-creature [init-c]
  (loop [c init-c
         remaining-time-points 100]
    (if (:alive c)
      (let [[new-c new-remaining-time-points] (exec-next c remaining-time-points)]
        (if (pos? new-remaining-time-points)
          (recur new-c
                 new-remaining-time-points)
          new-c))
      c)))

(defn tick [creatures]
  (prn :tick creatures)
  (for [creature creatures]
    (tick-creature creature)))

(defn tick-times [creatures times]
  ;;(prn :tick-times creatures times)
  (loop [cs creatures
         i times]
    (if (pos? i)
      (recur (doall (tick cs))
             (dec i))
      cs)))

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