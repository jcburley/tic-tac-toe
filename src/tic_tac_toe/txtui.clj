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

(defn- winner-to-string [s]
  (str "Player "
       (if (= (first s) :X) "X" "O")
       " has won! See: " (first (first (rest s)))))

(defn- status-to-string [s]
  (cond
    (nil? s) "Ongoing."
    (= :draw s) "The game is a draw."
    (= :X (first s)) (winner-to-string s)
    (= :O (first s)) (winner-to-string s)
  ))

(defn print-game-status
  "Use ASCII art, sorta, to show the game's current status; return suitable prompt"
  [g]
  (let [s (game/status g)]
    (println "\nGame board:\n")
    (println (str " " (c 1) (c 2) (c 3) "  123"))
    (println (str " " (c 4) (c 5) (c 6) "  456"))
    (println (str " " (c 7) (c 8) (c 9) "  789"))
    (println "\nGame status:" (status-to-string s))
    (str
     "'quit', 'start', 'resign'"
     (if (nil? s)
       (str
        ", or valid move (any one of: "
        (apply str (map #(char (+ 48 %)) (game/valid-moves (:board g))))
        ")")
       "")
     "> ")
    ))
