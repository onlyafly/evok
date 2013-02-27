(ns evok.mutation-spec
  (:use [speclj.core]
        [evok.mutation]))

(describe "rand-in-range"
  (it "result in range"
    (dorun (map (fn [_]
                  (let [result (rand-in-range 10 20)]
                    (should (and (<= 10 result)
                                 (<= result 20)))))
                (range 1000)))))