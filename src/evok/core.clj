(ns evok.core
  (:require (evok [board :as board]
                  [creature :as creature]
                  [tick :as tick]))
  (:gen-class))

(defn -main [& args]
  (println "Hello, World!")
  (binding [board/*size* [40 60]]
    (loop [creatures [(creature/genesis)]]
      (println (board/display-board creatures))
      (let [input (read-line)]
        (if (pos? (count input))
          (let [parsed-input (read-string input)]
            (cond
             
             (= parsed-input 'q)
             nil
             
             (number? parsed-input)
             (recur (tick/tick-times creatures parsed-input))

             :else
             (do
               (println "Unrecognized input")
               (recur creatures))))
          (recur (tick/tick creatures)))))))
