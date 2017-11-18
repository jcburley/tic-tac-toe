(ns tic-tac-toe.brutewin
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(defn- worst
  "Given a game and list of valid moves, return the worst move the opponent can possibly make to hurt the player originally calling 'move'"
  [g ms]
  (if (seq ms)
    (let [vm (best g ms)
          v (get vm 1 nil)]
      (when (> game/*verbose* 2)
        (println "In brutewin/worst: " g "\n  " vm v)
        (flush))
      (first vm))
    0))

(defn- my-turn
  "Given a game and a move, return a two-item list with the 'rating' of the move and the move itself"
  [g m]
  (let [me (:next-player g)
        new-g (game/after-move g m)
        s (:state new-g)]
    (list
     (cond
       (nil? s) (worst new-g (game/valid-moves new-g))
       (= :draw s) 0
       (= me (first s)) Integer/MAX_VALUE)
     m)))

(defn- best
  "Given a game and list of valid moves, return the best move to make"
  [g ms]
  (take 1 (sort #(> (first %1) (first %2)) (map #(my-turn g %) ms))))

(defn- best-move
  "Given a game and list of valid moves, return the best move to make"
  [g ms]
  (and (seq ms) (nth (first (best g ms)) 1)))

(defn move
  "Return a empty cell's index, or nil if nothing available. The cell is chosen to provide the best opportunity for the same player to win, or at least draw, via brute-force analysis."
  [g]
  (when (> game/*verbose* 1)
    (println "In brutewin/move: " g)
    (flush))
  (or (best-move g (game/valid-moves g))
      (do
        (println "\nNo further moves are available.\n")
        nil)))
