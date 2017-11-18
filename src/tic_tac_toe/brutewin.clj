(ns tic-tac-toe.brutewin
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(defn- my-turn
  "Given a game and a move, return a two-item list with the 'rating' of the move and the move itself"
  [g m]
  (let [me (:next-player g)
        new-g (game/after-move g m)
        s (:state new-g)]
    (list
     (cond
       (nil? s) Integer/MIN_VALUE  ; TODO: try possible opponent moves here
       (= :draw s) 0
       (= me (first s)) Integer/MAX_VALUE)
     m)))

(defn- best
  "Given a game and list of valid moves, return the best move to make"
  [g ms]
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
