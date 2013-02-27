(ns evok.beagle-spec
  (:use [speclj.core]
        [evok.beagle]))

(describe "build"
  (it "base case"
    (should= []
             (build []))))