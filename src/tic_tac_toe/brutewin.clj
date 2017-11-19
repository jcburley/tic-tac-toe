(ns tic-tac-toe.brutewin
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

(declare best)

;; A macro that expands into multiple sexpr would be helpful for
;; :post, because then a failed post-condition would highlight the
;; specific condition that failed (one would hope), rather than just
;; saying 'valid-rated-move? failed'. But Lisp macros generally, and
;; Clojure macros specifically, can expand into only a single sexpr.

;; (defmacro valid-rated-move-as-macro?
;;   "Suitable for pre/post-conditions"
;;   [rm]
;;   `((list? ~rm)
;;     (= 3 (count ~rm))
;;     (integer? (first ~rm))
;;     (integer? (first (rest ~rm)))))

(defn- valid-rated-move?
  "Suitable for pre/post-conditions"
  [rm]
  (or (and (list? rm)
        (= 2 (count rm))
        (integer? (first rm))
        (integer? (first (rest rm))))
      (do
        (printf "Failed %%=%s\n" (pr-str rm))
        (flush)
        false)))

(defn- worst
  "Given a game and non-empty list of valid moves, return a two-item list with the 'rating' of/and the worst move the opponent can possibly make to hurt the player originally calling 'move'"
  [g ms]
  {:pre [(seq ms)]
   :post [(valid-rated-move? %)]}
  (when (> game/*verbose* 0)
    (printf "In brutewin/worst: %s\n  Valid moves for %s: %s\n"
            (pr-str g) (:next-player g) (pr-str ms))
    (flush))
  (let [best-m (best g ms)]
    (when (> game/*verbose* 0)
      (printf "In brutewin/worst, best %s move: %s\n"
              (:next-player g) best-m)
      (flush))
    (condp = (first best-m)
      nil? best-m                                                ; Should never happen in tic-tac-toe (unless we limit the depth of the search)
      Integer/MAX_VALUE (list Integer/MIN_VALUE (nth best-m 1))  ; Opponent wins means "we" lose
      Integer/MIN_VALUE (list Integer/MAX_VALUE (nth best-m 1))  ; Should never happen in tic-tac-toe
      (list (- (first best-m)) (nth best-m 1)))))                ; Invert the rating

(defn- my-turn
  "Given a game and a move, return a two-item list with the 'rating' of the move and the move itself"
  [g m]
  {:post [(valid-rated-move? %)]}
  (let [me (:next-player g)
        new-g (game/after-move g m)
        new-s (:state new-g)
        new-ms (game/valid-moves new-g)]
    (when (> game/*verbose* 0)
      (printf "In brutewin/my-turn: %s\n  Valid moves for %s: %s\n"
              (pr-str new-g) (:next-player new-g) (pr-str new-ms))
      (flush))
    (cond
      (nil? new-s) (worst new-g new-ms)                   ; "Inverse" rating of the opponent's best next move.
      (= :draw new-s) (list 0 m)                          ; Outright draw, zero rating.
      (= me (first new-s)) (list Integer/MAX_VALUE m))))  ; Outright win, max possible rating.

(defn- best
  "Given a game and list of valid moves, return a two-item list with the 'rating' of/and best move to make; return '(nil nil) if no valid moves"
  [g ms]
  {:post [(or (= '(nil nil) %) (valid-rated-move? %))]}
  (when (> game/*verbose* 0)
    (println "In brutewin/best:" g "\n  Valid moves:" ms)
    (flush))
  (if (seq ms)
    (first (sort #(> (first %1) (first %2)) (map #(my-turn g %) ms)))
    '(nil nil)))

(defn- best-move
  "Given a game and a list of valid moves, return the best move to make or nil if no valid moves"
  [g ms]
  (nth (best g ms) 1))

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
