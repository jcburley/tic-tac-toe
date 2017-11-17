(ns tic-tac-toe.game)

(def ^:private winning-positions
  [[1 2 3]
   [4 5 6]
   [7 8 9]
   [1 4 7]
   [2 5 8]
   [3 6 9]
   [1 5 9]
   [3 5 7]])

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

(defn board-cell
  "Get whatever is in the cell at board position N (1-based)"
  [board idx]
  (get board (dec idx)))

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
