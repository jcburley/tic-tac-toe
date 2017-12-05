(ns tic-tac-toe.next
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(defn move
  "Return the next empty cell's index, or nil if nothing available"
  [g]
  (when (> game/*verbose* 1)
    (println "In next/move: " g)
    (flush))
  (or (first (game/valid-moves g))
      (do
        (println "\nNo further moves are available.\n")
        nil)))
