(ns tic-tac-toe.brutewin
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(defn- my-turn [g m]
  (list (inc m) m))

(defn- best [g ms]
  (nth (first (take 1 (sort #(> (first %1) (first %2)) (map #(my-turn g %) ms)))) 1))

(defn move
  "Return a empty cell's index, or nil if nothing available. The cell is chosen to provide the best opportunity for the same player to win, or at least draw, via brute-force analysis."
  [g]
  (when (> game/*verbose* 1)
    (println "In brutewin/move: " g)
    (flush))
  (or (best g (game/valid-moves g))
      (do
        (println "\nNo further moves are available.\n")
        nil)))
