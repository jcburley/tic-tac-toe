(ns tic-tac-toe.txtui
  (:require
   [clojure.string :as s]
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(defn- cell-to-char
  "'X', 'O', or nil, depending on the value of the cell in the board"
  [board idx]
  (condp = (game/board-cell board idx)
    :X "X"
    :O "O"
    nil "-"))

(defmacro c [idx]
  `(cell-to-char (:board ~'g) ~idx))

(defn status-to-string [s]
  (cond
    (nil? s) "Ongoing."
    (= :draw s) "The game is a draw."
    (= :X (first s)) "Player X has won!"
    (= :O (first s)) "Player O has won!"
  ))

(defn print-game-status
  "Use ASCII art, sorta, to show the game's current status; return suitable prompt"
  [g]
  (let [s (game/status g)]
    (println "\nGame board:\n")
    (println (str " " (c 1) (c 2) (c 3) "  123"))
    (println (str " " (c 4) (c 5) (c 6) "  456"))
    (println (str " " (c 7) (c 8) (c 9) "  789"))
    (println "\nGame status:" (status-to-string s))))
