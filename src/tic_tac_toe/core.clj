(ns tic-tac-toe.core
  (:gen-class))

(defn parse-opts
  ""
  [fn args]
  (if (some (partial = "--help") args)
    (println "Just try it.")
    (fn args)))

(defn- board-indices
  "Return indices for a board"
  []
  '(1 2 3 4 5 6 7 8 9))

(defrecord Game [state next-player board])

(defprotocol Player
  "Define interface for fns that implement (or talk with) a player"
  ; Choose next move for the player, based on Game.
  (choose [this])
  ; Player should be advised as to valid moves.
  (wants-valid-moves? [this])
  ; Report state of Game to player. 'why' is :move, :retry, :win,
  ; :lose, or :draw; 'what' is nil or { <player-kw> <move> }; 'valid'
  ; is optional set of valid moves (1-9), or nil if not provided.
  (report [this why what valid]))

(defn valid-move?
  "Return nil if proposed move is invalid, true if valid"
  [board move]
  (or (= move :resign)
      (and
       (integer? move)
       (nil? (get board move true)))))  ; Cell occupied?

(defn valid-moves
  "Returns set of valid moves for the player to move next"
  [board]
  ; TODO: simplify this filter, if possible
  (filter (fn [n] (nil? (get board n))) (board-indices)))

(defn read-move-interactive
  "Read a move from the terminal and parse it, then returns the move, or nil if invalid"
  [game valid-fn?]
  (let [m (read-line)
        value (try
                (Integer/parseInt m)
                (catch NumberFormatException e m))]
    (cond
      (= m "resign")
      :resign
      (valid-fn? game value)
      value
      :else nil)))

(comment (do
     (println "Invalid input; please enter valid move or 'resign'.")
     (flush)
     (recur))


; determine-move-randomly picks a random valid move, but never :resign.


; next-valid-move takes a fn that determines or reads a move, a fn
; that validates the move, a set of valid board moves for a player,
; calls the fn repeatedly until a non-nil result is returned that is
; validated, and ultimately returns the valid move.


; update-game returns a new game based on a existing game plus a move,
; and returns :draw, an player (:X or :O), or a set of valid next
; moves and player to make the move.


(defn- new-board
  "Return new, empty, tic-tac-toe board"
  []
  (vec (map (fn [x] constantly nil) (board-indices))))

(defn new-game
  "Return new tic-tac-toe game with empty board and :X as next player to move."
  []
  (Game. nil :X (new-board)))





(defn start-game
  ""
  [args]
  (println "Hello, World! It's me!! And I still don't play tic-tac-toe!!")
  (do
    (doall (for [arg args] (println arg)))
    :success))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (parse-opts start-game args))
