(ns evok.board-spec
  (:require (evok [board :as board]
                  [creature :as creature]))
  (:use [speclj.core]
        [evok.board]))

(describe "build-board"
  (it "simple"
    (binding [board/*size* [3 3]]
      (should= [["g" "-" "-"]
                ["-" "-" "-"]
                ["-" "-" "-"]]
               (build-board [(creature/genesis)])))))

(describe "display-board"
  (it "shows the board"
    (binding [board/*size* [3 3]]
      (should= (str "g--\n"
                    "---\n"
                    "---")
               (display-board [(creature/genesis)])))))