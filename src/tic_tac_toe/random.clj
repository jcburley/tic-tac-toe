(ns tic-tac-toe.random
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(defn move
  "Return a random empty cell's index, or nil if nothing available."
  [g]
  (when (> game/*verbose* 1)
    (println "In random/move: " g)
    (flush))
  (or (rand-nth (game/valid-moves g))
      (do
        (println "\nNo further moves are available.\n")
        nil)))
