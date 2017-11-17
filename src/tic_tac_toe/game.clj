(ns tic-tac-toe.game)

(def ^:dynamic *verbose*)

(defn- board-indices
  "Return indices for a board"
  []
  '(1 2 3 4 5 6 7 8 9))

(defrecord Game [state next-player board])

(def ^:private winning-positions
  [[1 2 3]
   [4 5 6]
   [7 8 9]
   [1 4 7]
   [2 5 8]
   [3 6 9]
   [1 5 9]
   [3 5 7]])

(defn valid-moves
  "Returns set of valid moves for the player to move next"
  [board]
  ;; TODO: simplify this filter, if possible
  (filter (fn [n] (nil? (get board n))) (board-indices)))

(defn- winner-status
  "Return :X or :O if the player has won the triad"
  [player w]
  (not (some #(not= player %) w)))

(declare board-cell)

(defn- winner-status-for-triad-positions
  "Returns whether the player occupies all triad positions on the board"
  [board player triad-positions]
  (winner-status player
                 (map #(board-cell board %) triad-positions)))

(defn- winning-triad-positions-for-player
  "Return list of winning positions 'owned' by given player"
  [board player]
  (map #(first (rest %))
       (filter #(first %)
               (map #(list
                      (winner-status-for-triad-positions board player %)
                      %)
                    winning-positions))))

(defn- winners-for-board
  "Return map of players with list of winning positions 'owned' by each player"
  [board]
  {:X (winning-triad-positions-for-player board :X)
   :O (winning-triad-positions-for-player board :O)})

(defn- new-board
  "Return new, empty, tic-tac-toe board"
  []
  (vec (map (fn [x] constantly nil) (board-indices))))

(defn new
  "Return new tic-tac-toe game with empty board and :X as next player to move."
  []
  (Game. nil :X (new-board)))

(defn board-cell
  "Get whatever is in the cell at board position N (1-based)"
  [board idx]
  (get board (dec idx)))

(defn valid-move?
  "Return nil if proposed move is invalid, true if valid"
  [board move]
  (or (= move :resign)
      (and
       (integer? move)
       (nil? (get board (dec move) true))))) ;; Cell occupied?

(defn status
  "Returns :draw, (:X <list-of-winning-positions>) (meanings :X
  wins), (:O <list-of-winning-positions>) (:O wins), or nil (game not
  over)"
  [g]
  (let [w (winners-for-board (:board g))]
    (cond
      (> (count (:X w))(count (:O w)))
      (list :X (:X w))
      (> (count (:O w))(count (:X w)))
      (list :O (:O w))
      (some nil? (:board g)) nil
      :else :draw)))

(defn after-move
  "Return new game object after applying a specific move"
  [g m]
  (let [b (assoc (:board g) (dec m) (:next-player g))
        p (if (= :X (:next-player g)) :O :X)
        s (status g)
        new-g (Game. s p b)]
    (if (> *verbose* 0)
      (println new-g))
    new-g))
