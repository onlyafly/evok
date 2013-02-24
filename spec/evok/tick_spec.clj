(ns evok.tick-spec
  (:require (evok [creature :as creature]
                  [board :as board]))
  (:use [speclj.core]
        [evok.tick]))

(describe "exec"
  (it "number"
    (let [c1a {:stack []}
          c1b {:stack [42]}]
      (should= c1b
               (exec c1a 42))))
  (it "move-random"
    (with-redefs [shuffle (fn [coll] coll)
                  rand-int (fn [_] 0)]
      (binding [board/*size* [3 3]]
        (let [c1a {:pos [0 0]}
              c1b {:pos [1 0]}]
          (should= c1b
                   (exec c1a :move-random)))))))

(describe "exec-next"
  (it "number"
    (let [c1a {:pointer 0
               :energy 100
               :code [42]
               :stack []}
          c1b {:pointer 0
               :energy 100
               :code [42]
               :stack [42]}]
      (should= [c1b 99]
               (exec-next c1a 100))))
  (it "move-random"
    (with-redefs [shuffle (fn [coll] coll)
                  rand-int (fn [_] 0)]
      (binding [board/*size* [3 3]]
        (let [c1a {:stack []
                   :energy 100
                   :code [:move-random]
                   :pointer 0
                   :pos [0 0]}
              c1b {:stack []
                   :energy 0
                   :code [:move-random]
                   :pointer 0
                   :pos [1 0]
                   :display "*"
                   :alive false}]
          (should= [c1b 0]
                   (exec-next c1a 100)))))))

(describe "tick-creature"
  (it "simple"
    (with-redefs [shuffle (fn [coll] [dec])
                  rand-int (fn [_] 0)]
      (binding [board/*size* [3 3]]
        (let [ca (creature/genesis)
              cb (assoc ca
                   :energy 99900)]
          (should= cb
                   (tick-creature ca)))))))