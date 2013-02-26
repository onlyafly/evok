(ns evok.core
  (:require (evok [world :as world]))
  (:gen-class))

(defn -main [& args]
  (println "Hello, World!")
  (world/init)
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

           ;; Show details about a cell
           (and (vector? parsed-input)
                (= 2 (count parsed-input)))
           (do
             (prn @(world/location-by-coord parsed-input))
             (recur))

           ;; Unrecognized input
           :else
           (do
             (println "Unrecognized input:")
             (prn parsed-input)
             (recur))))
        (do
          (world/tick)
          (recur))))))
