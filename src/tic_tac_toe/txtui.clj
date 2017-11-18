(ns tic-tac-toe.txtui
  (:require
   [tic-tac-toe.game :as game]
   [tic-tac-toe.next :as next]
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
     "Enter command (try 'help')"
     (if (nil? s)
       (str
        ", or valid move (any one of: "
        (apply str (map #(char (+ 48 %)) (game/valid-moves g)))
        ")")
       "")
     "> ")
    ))

(defn- help []
  (println
   "\n"
   "Commands include:\n\n"
   "next      -- Choose next available cell\n"
   "random    -- Choose random available cell\n"
   "brutewin  -- Try to win via brute-force analysis\n"
   "resign    -- Just like 'quit'\n"
   "back      -- Back up one move\n"
   "reset     -- Reset to beginning of game\n"
   "start     -- Start a new game (just like 'reset' but wastes memory)\n"
   "help      -- This message\n"
   "quit      -- Exit the game\n"))

(defn read-move-interactive
  "Read a move from the terminal and parse it, then returns the move, or nil if invalid"
  [g valid-fn?]
  (let [m (read-line)
        value (try
                (Integer/parseInt m)
                (catch NumberFormatException e m))]
    (cond
      (= m "next") (next/move g)
      (= m "random") :random
      (= m "brutewin") :brutewin
      (= m "resign") :resign
      (= m "quit") :quit
      (= m "start") :start
      (= m "back") :back
      (= m "reset") :reset
      (= m "help") (do
                     (help)
                     :help)
      (valid-fn? (:board g) value) value
      :else (do
              (println "\nInvalid move, try again.\n")
              nil))))
