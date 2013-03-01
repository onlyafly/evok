;; Describes the F21 CPU, with instructions based on Forth:
;; http://www.ultratechnology.com/f21cpu.html#cpu
;;
;; Discusses the minimal instruction set for Turing Completeness:
;; http://cs.stackexchange.com/a/995
;;
;; Definitions:
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

(def command-marker 0)

;;                            Instruction      TimePoints     EnergyPoints
(def instruction-descriptors [[:nop            1              0]   ; zero is also the command marker
                              [:startblock     1              1]
                              [:endblock       1              1]
                              [:if             1              1]
                              [:display        100            100]
                              [:turn           100            100]
                              [:move           100            100]
                              [:procreate      0              100] ; energy cost calculated dynamically
                              [:eat            100            100]
                              ])

(def energy-point-table (into {} (for [d instruction-descriptors]
                                   [(nth d 0) ; instruction keyword
                                    (nth d 2) ; energy points
                                    ])))
(def time-point-table (into {} (for [d instruction-descriptors]
                                 [(nth d 0) ; instruction keyword
                                  (nth d 1) ; time points
                                  ])))
(def opcode->instruction-table (into {} (for [i (range (count instruction-descriptors))]
                                  (let [d (nth instruction-descriptors i)]
                                    [i         ; opcode
                                     (nth d 0) ; instruction keyword
                                     ]))))

(def instruction->opcode-table (clojure.set/map-invert opcode->instruction-table))

;;---------- Decoding

(defn raw-value->instruction [raw-value]
  (let [opcode (util/as-bounded-integer raw-value (count instruction-descriptors))]
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
        (recur result
               x
               xs)))    
    ;; No raw values left
    (if prev
      (conj result prev)
      result)))

(defn unbuild [raw-code]
  (unwrap [] nil raw-code))