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

(declare board-cell)

(defn valid-moves
  "Returns set of valid moves for the player to move next"
  [g]
  ;; TODO: simplify this filter, if possible
  (if (nil? (:state g))
    (filter
     (fn [n] (nil? (board-cell (:board g) n)))
     (board-indices))))

(defn- winner-status
  "Return :X or :O if the player has won the triad"
  [player w]
  (not (some #(not= player %) w)))

(defn- winner-status-for-triad-positions
  "Returns whether the player occupies all triad positions on the board"
  [b player triad-positions]
  (winner-status player
                 (map #(board-cell b %) triad-positions)))

(defn- winning-triad-positions-for-player
  "Return list of winning positions 'owned' by given player"
  [b player]
  (map #(first (rest %))
       (filter #(first %)
               (map #(list
                      (winner-status-for-triad-positions b player %)
                      %)
                    winning-positions))))

(defn- winners-for-board
  "Return map of players with list of winning positions 'owned' by each player"
  [b]
  {:X (winning-triad-positions-for-player b :X)
   :O (winning-triad-positions-for-player b :O)})

(defn- new-board
  "Return new, empty, tic-tac-toe board"
  []
  (vec (map (fn [x] constantly nil) (board-indices))))

(defn new
  "Return new tic-tac-toe game with empty board and :X as next player to move"
  []
  (Game. nil :X (new-board)))

(defn board-cell
  "Get whatever is in the cell at board position N (1-based)"
  ([b idx]
   (board-cell b idx nil))
  ([b idx dflt]
   (get b (dec idx) dflt)))

(defn valid-move?
  "Return nil if proposed move is invalid, true if valid"
  [b move]
  (or (= move :resign)
      (and
       (integer? move)
       (nil? (board-cell b move true))))) ;; Cell occupied?

(defn- board-status
  "Returns :draw, (:X <list-of-winning-positions>) (meanings :X
  wins), (:O <list-of-winning-positions>) (:O wins), or nil (game not
  over)"
  [b]
  (let [w (winners-for-board b)]
    (cond
      (> (count (:X w))(count (:O w)))
      (list :X (:X w))
      (> (count (:O w))(count (:X w)))
      (list :O (:O w))
      (some nil? b) nil
      :else :draw)))

(defn status
  "Returns :draw, (:X <list-of-winning-positions>) (meanings :X
  wins), (:O <list-of-winning-positions>) (:O wins), or nil (game not
  over)"
  [g]
  (board-status (:board g)))

(defn after-move
  "Return new game object after applying a specific move"
  [g m]
  (let [b (assoc (:board g) (dec m) (:next-player g))
        p (if (= :X (:next-player g)) :O :X)
        s (board-status b)
        new-g (Game. s p b)]
    (if (> *verbose* 0)
      (println new-g))
    new-g))
