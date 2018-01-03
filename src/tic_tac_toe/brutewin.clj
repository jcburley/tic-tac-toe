(ns tic-tac-toe.brutewin
  (:require
   [tic-tac-toe.game :as game]
   )
  (:gen-class))

;; This module implements an "ideal" strategy to do its best (win or
;; draw versus lose) via brute-force analysis, which can be quite
;; expensive. It is called for a single move, so it needn't be called
;; for each move made by a particular player. It does the analysis
;; dynamically (on the fly), rather than creating a map of possible
;; strategies and consulting it -- though, since it uses only the
;; immutable datatypes offered by Clojure, presumably what it walks
;; the first time could be memoized.
;;
;; tic-tac-toe has certain specific properties. For example, strictly
;; speaking, one cannot make a move that causes one to lose; the
;; opponent must move to win. (Compare to Chess, where one can move a
;; piece such that one's own king ends up in checkmate, which -- if
;; that was allowed, and I don't think it actually is -- would mean
;; one can make a losing move.)
;;
;; But if the rules were just slightly modified to allow a player to
;; place the other player's piece (mark) on the board, player X could
;; explicitly lose by putting an O in the 3rd of a winning play.
;;
;; Anyway, for simple two-person, non-random, single-move games, a
;; goal is to keep this module (as well as some others) abstracted
;; away from the specifics of any one game. Such abstraction is TBD
;; (TODO) for now.
;;
;; Determining the ideal brute-force algorithm is requiring a lot of
;; thought on my part, as I can't recall figuring one out for many,
;; many years, and am not sure when, where, why, or in what language I
;; did it way back then!
;;
;; Starting from the lowest level and working up, a "leaf" move (M) is
;; one for the player (P) that results in a win, loss, or draw --
;; there is no continuation of play. So, a leaf move under
;; consideration has one of these results:
;;
;;   P(M) => Win (W); Draw (D); Lose (L).
;;
;; Given multiple moves that are available, they are to be considered
;; in the order shown (choose W over D, and D over L). E.g. if P(M) =>
;; W, the algorithm could immediately exit with that as the desired
;; move -- in any case, some W-resulting move should be returned, if
;; at least one such move exists. If not, some D-resulting move should
;; be returned; etc.
;;
;; Now, a non-leaf move represents a further possibility, in that the
;; opponent (Q) is then able to make a move (M'). Depending on the
;; results of the opponent's available moves, P might or might not
;; want to make that non-leaf move, versus making a leaf move, by
;; considering the available options in this order, where "(> 0 n)"
;; indicates that at least one move is available that results in <n>:
;;
;;   P(M) => Q(M') => (> 0 W) => P should avoid M at all costs.
;;
;;   P(M) => Q(M') => (> 0 D) => P should prefer a W move to M.
;;
;;   P(M) => Q(M') => (> 0 L) => P should choose M.
;;
;; When considering a number of possible moves of equal outcome (in
;; the boolean sense), P might wish to prefer moves that offer a
;; higher probability of winning (or drawing over losing), if (say) Q
;; is making random moves, and maybe even if "future P" does so as
;; well.
;;
;; Towards that end, the counts of each type of result for Q(M')
;; should be "rolled up" so P can choose among the options for M.
;; Given <w> Q(M')=>W results map to P(M)=><w>*L (<w> ways to lose)
;; results for P making move M, etc.
;;
;; Incorporating such possibilities when analyzing moves for P:
;;
;;   P(M) => 1*W  ;; outright win
;;
;;   P(M) => <n>*W, 0*D, 0*L  ;; no way to lose or draw, <n> ways to win
;;
;;   P(M) => 1*D  ;; outright draw
;;
;;   P(M) => <n>*W, <p>*D, 0*L  ;; no way to lose, <p> ways to draw
;;
;;   P(M) => 1*L  ;; outright loss
;;
;;   P(M) => <n>*W, <p>*D, <w>*L  ;; <w> ways to lose
;;
;; <n>*W and <p>*D are pulled in to choose between similarly non-ideal
;; moves -- e.g. if there's no M available that won't give Q at least
;; one winning M', P should choose M that offer the most likelihood of
;; Q making a bad (losing or draw) M' and having the fewest ways to
;; win.
;;
;; Of course, Q might well have a non-leaf M' available, further
;; complicating things. From Q's perspective, the above analysis works
;; (with P being its Q); so, a recursive call to the analyzer can be
;; performed, with the results rolled up (inverted from P's point of
;; view) into the "report" returned for the analysis of Q's choices.
;; These results are then rolled up into the report on P(M).
;;

;; Recursive analysis is used for such Ongoing moves. The next
;; possible move (by the opponent) is analyzed to see what the results
;; would be for each move. This analysis starts as above ("Starting
;; from the lowest level...") but its result is modified to represent
;; the interest of the original mover:
;;
;;   Win => Certain loss for original mover, so avoid the original
;;     move at all costs.
;;
;;   Draw => Possible draw for original mover.
;;
;;   Lose => Possible win for original mover.
;;
;; Note that the "Win => Certain loss" result effectively
;; "short-circuits" the analysis of other potential opponent moves --
;; they need not be analyzed, since they cannot improve the
;; favorability of the original move under consideration, unless one
;; is relying on the opponent *not* making a winning move.
;;
;; The remaining possibilities require their aggregation across the
;; possible moves on the part of the opponent:
;;
;;   all Lose => Sure win for original mover.
;;
;;   all Draw => Sure draw for original mover.
;;
;; TODO: FIX/FINISH DRAFT LOGIC, ABOVE, AND IMPLEMENT.
;;
;; BUG: Given moves 12685, O should choose 4, but chooses 3. X then
;; wins with 4.
;;
;; BUG: Given moves 918, O should 7 but 2, then X should 7 but 3.
;;
;; BUG: Given 59, X should not 1, but 1.

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
    (list
     (cond
       (nil? new-s) (first (worst new-g new-ms)); "Inverse" rating of the opponent's best next move.
       (= :draw new-s) 0                        ; Outright draw, zero rating.
       (= me (first new-s)) Integer/MAX_VALUE)  ; Outright win, max possible rating.
     m)))

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
