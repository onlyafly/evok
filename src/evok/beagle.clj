;; Describes the F21 CPU, with instructions based on Forth:
;; http://www.ultratechnology.com/f21cpu.html#cpu
;;
;; Discusses the minimal instruction set for Turing Completeness:
;; http://cs.stackexchange.com/a/995
;;
;; Definitions:
;;
;;  instruction = A command executed by the Evok VM, expressed as a
;;                Clojure keyword
;;  opcode = An integer value representing an instruction
;;  command marker = The integer value signifying that the preceding
;;                   raw value should be interpreted as an opcode
;;  Beagle code = Human readable language consisting of instructions
;;                as Clojure keywords and data as integer values.
;;  raw code = Compiled Beagle code comprised of a sequence of raw
;;             values (integers)

(ns evok.beagle
  (:require clojure.set
            (evok [util :as util])))

(declare build-item)

;;---------- Constants

(def maximum-stack-size 100)
(def trimmed-stack-size 50)

;;---------- Instruction information

(def command-marker 0)

;;                            Instruction      Logic?     TimePoints     EnergyPoints
(def instruction-descriptors [[:nop            false      1              0]   ; zero is also the command marker
                              [:jump           true       1              1]
                              [:call           true       1              1]
                              [:ret            true       1              1]
                              [:jumpif         true       1              1]
                              [:rand           true       1              1]
                              [:turn           false      100            100]
                              [:move           false      100            100]
                              [:procreate      false      0              100] ; energy cost calculated dynamically
                              [:eat            false      100            100]
                              [:smell          false      10             10]
                              ])

(def logic-instructions (set (map #(nth % 0) ; instruction keyword
                                  (filter (fn [desc]
                                            (nth desc 1)) ; logic instruction?
                                          instruction-descriptors))))

(def energy-point-table (into {} (for [d instruction-descriptors]
                                   [(nth d 0) ; instruction keyword
                                    (nth d 3) ; energy points
                                    ])))
(def time-point-table (into {} (for [d instruction-descriptors]
                                 [(nth d 0) ; instruction keyword
                                  (nth d 2) ; time points
                                  ])))
(def opcode->instruction-table (into {} (for [i (range (count instruction-descriptors))]
                                  (let [d (nth instruction-descriptors i)]
                                    [i         ; opcode
                                     (nth d 0) ; instruction keyword
                                     ]))))

(def instruction->opcode-table (clojure.set/map-invert opcode->instruction-table))

;;---------- Decoding

(defn raw-value->instruction [raw-value]
  (let [opcode (util/interpret-to-bound raw-value (count instruction-descriptors))]
    (opcode->instruction-table opcode)))

;;---------- Building

(defmulti build-item-method (fn [x] (cond (integer? x) :RAWVALUE
                                         (keyword? x) :INSTRUCTION
                                         :else :UNKNOWN)))
(defmethod build-item-method :INSTRUCTION [instruction]
  [(instruction->opcode-table instruction) command-marker])
(defmethod build-item-method :RAWVALUE [n]
  [n])
;; REFACTOR this can be removed
(comment (defmethod build-item-method :VECTOR [item]
           (vec (concat (build-item :startblock)
                        (mapcat build-item item)
                        (build-item :endblock)))))

(defn build-item [item]
  {:post [(vector? %)]}
  (build-item-method item))

(defn build [beagle-code]
  (vec (mapcat build-item beagle-code)))

;;---------- Unbuilding

(defn unwrap [result prev more]
  (if (pos? (count more))
    ;; More raw values
    (let [[x & xs] more]
      (if (zero? x)
        ;; Next raw value is a command marker
        (if prev
          (recur (conj result (raw-value->instruction prev))
                 nil
                 xs)
          (recur result
                 x
                 xs))
        ;; Next raw value is a non-command marker
        (if prev
          (recur (conj result prev)
                 x
                 xs)
          (recur result
                 x
                 xs))
        ))    
    ;; No raw values left
    (if prev
      (conj result prev)
      result)))

(defn unbuild [raw-code]
  (unwrap [] nil raw-code))

;;---------- Execution helpers

(defn random-raw-value []
  (rand-int 100000))

(defn interpret-as-address [n m]
  (util/interpret-to-bound n (count (:code m))))

(defn interpret-as-boolean [n]
  (zero? (util/interpret-to-bound n 2)))

(defn- machine? [machine]
  ;; TODO
  true)

(defn mpop [m stack-name]
  {:pre [(keyword? stack-name)
         (contains? #{:rstack :dstack} stack-name)]}
  (prn :mpop (:uid m) stack-name :stack (stack-name m))
  (if (pos? (count (stack-name m)))
    (update-in m [stack-name] pop)
    m))

;; FIX should commands that need a value from the stack use a random
;; value when the stack is empty or should they fail?
(defn mpeek [m stack-name]
  {:pre [(keyword? stack-name)
         (contains? #{:rstack :dstack} stack-name)]}
  (prn :mpeek (:uid m) stack-name :stack (stack-name m))
  (if (pos? (count (stack-name m)))
    (peek (stack-name m))
    (random-raw-value)))

(defn mpush [m stack-name val]
  {:pre [(keyword? stack-name)
         (contains? #{:rstack :dstack} stack-name)]}
  (prn :mpush (:uid m) stack-name :stack (stack-name m) :val val)
  (-> (if (> (count (stack-name m))
             maximum-stack-size)
        (update-in m [stack-name] #(vec (take-last trimmed-stack-size %)))
        m)
      (update-in [stack-name] conj val)))

;;---------- Logic execution

(defmulti exec (fn [machine instruction] instruction))

;; Unconditional jump. Move to location specified by top of datastack
;; (raw value insterpreted as address within range of code length)
(defmethod exec :jump [m _]
  (let [destination (interpret-as-address (mpeek m :dstack) m)]
    (-> m
        (mpop :dstack)
        ;; Decrement so that after the pointer is incremented, it points
        ;; to the correct location
        (assoc :pointer (dec destination)))))

;; Push the pointer address + 1 to the rstack
(defmethod exec :call [m _]
  (let [destination-address (interpret-as-address (mpeek m :dstack) m)
        return-address (:pointer m)]
    (-> m
        (mpop :dstack)
        (mpush :rstack (inc return-address))
        ;; Decrement so that after the pointer is incremented, it points
        ;; to the correct location
        (assoc :pointer (dec destination-address)))))

;; Jump to the address popped off of the rstack
(defmethod exec :ret [m _]
  (let [return-address (interpret-as-address (mpeek m :rstack) m)]
    (-> m
        (mpop :rstack)
        ;; Decrement so that after the pointer is incremented, it points
        ;; to the correct location
        (assoc :pointer (dec return-address)))))

;; Pop two items off the dstack. If the top item is true, jump to the
;; address given by the second item.
(defmethod exec :jumpif [m _]
  (let [predicate (interpret-as-boolean (mpeek m :dstack))
        m1 (mpop m :dstack)
        address (interpret-as-address (mpeek m1 :dstack) m1)
        m2 (mpop m1 :dstack)]
    (if predicate
      ;; Decrement so that after the pointer is incremented, it points
      ;; to the correct location
      (assoc m2 :pointer (dec address))
      m2)))

;; Push a random value on the dstack
(defmethod exec :rand [m _]
  (prn :rand (:uid m))
  (mpush m :dstack (random-raw-value)))

(defn exec-instruction [machine instruction]
  {:post [(machine? %)]}
  (exec machine instruction))