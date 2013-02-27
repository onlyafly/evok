(ns evok.beagle-spec
  (:use [speclj.core]
        [evok.beagle]))

(describe "compile"
  (it "base case"
    (should= []
             (compile []))))