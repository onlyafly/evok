(ns evok.beagle-spec
  (:use [speclj.core]
        [evok.beagle]))

(describe "build"
  (it "base case"
    (should= []
             (build []))))

(describe "unbuild"
  (it "base case"
    (let [ba [:turn :turn :move :move :move :move :eat :procreate]
          ia (build ba)]
      (should= ba
               (unbuild ia)))))

