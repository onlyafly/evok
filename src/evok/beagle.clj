(ns evok.beagle)

(def instruction-table {0 :nop ;zero is also the command instruction
                        1 :nop
                        2 :turn
                        3 :move
                        4 :procreate
                        5 :eat})

(defn compile [code]
  [])