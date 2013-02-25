(ns evok.core
  (:require (evok [world :as world]))
  (:gen-class))

(defn -main [& args]
  (println "Hello, World!")
  (loop []
    (world/show!)
    (let [input (read-line)]
      (if (pos? (count input))
        (let [parsed-input (read-string input)]
          (cond

           ;; Quit
           (= parsed-input 'q)
           nil

           ;; Tick X times
           (number? parsed-input)
           (do
             (world/tick-times parsed-input)
             (recur))

           ;; Unrecognized input
           :else
           (do
             (println "Unrecognized input")
             (recur))))
        (do
          (world/tick)
          (recur))))))
