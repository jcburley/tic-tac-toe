(ns tic-tac-toe.core
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [tic-tac-toe.game :as game]
   [tic-tac-toe.txtui :as txtui]
   )
  (:gen-class))

(def ^:dynamic *verbose*)

(def cli-options
  ;; An option with a required argument
  [["-v" "--verbose" "Verbosity level"
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This is a primitive tic-tac-toe program. There are many like it, but this one is mine."
        ""
        "Usage: tic-tac-toe [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  start    Start a new game"
        ""
        "Please refer to the source code for more information."]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}

      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}

      ;; custom validation on arguments
      (and (= 1 (count arguments))
           (#{"start"} (first arguments)))
      {:action (first arguments) :options options}

      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit
  "Print message (if provided) then return {:exit <status>} to indicate the program should exit"
  ([status]
   (exit status nil))
  ([status msg]
   (when msg (println msg))
   {:exit status}))

;; My original hacky attempt to parse options:
;;(defn parse-opts
;;  ""
;;  [fn args]
;;  (if (some (partial = "--help") args)  <== TODO: learn the Clojure way to do this
;;    (usage)
;;    (fn args)))

(defn- board-indices
  "Return indices for a board"
  []
  '(1 2 3 4 5 6 7 8 9))

(defrecord Game [state next-player board])

(defprotocol Player
  "Define interface for fns that implement (or talk with) a player"
  ;; Choose next move for the player, based on Game.
  (choose [this])
  ;; Report state of Game to player. 'why' is :move, :retry, :win,
  ;; :lose, or :draw; 'what' is nil or { <player-kw> <move> }; 'valid'
  ;; is set of valid moves (1-9).
  (report [this why what valid]))

(defn valid-move?
  "Return nil if proposed move is invalid, true if valid"
  [board move]
  (or (= move :resign)
      (and
       (integer? move)
       (nil? (get board (dec move) true))))) ;; Cell occupied?

(defn valid-moves
  "Returns set of valid moves for the player to move next"
  [board]
  ;; TODO: simplify this filter, if possible
  (filter (fn [n] (nil? (get board n))) (board-indices)))

(defn read-move-interactive
  "Read a move from the terminal and parse it, then returns the move, or nil if invalid"
  [game valid-fn?]
  (let [m (read-line)
        value (try
                (Integer/parseInt m)
                (catch NumberFormatException e m))]
    (cond
      (= m "resign") :resign
      (= m "quit") :quit
      (= m "start") :start
      (valid-fn? (:board game) value) value
      :else nil)))

(comment (do
           (println "Invalid input; please enter valid move or 'resign'.")
           (flush)
           (recur)))


;; determine-move-randomly picks a random valid move, but never :resign.


;; next-valid-move takes a fn that determines or reads a move, a fn
;; that validates the move, a set of valid board moves for a player,
;; calls the fn repeatedly until a non-nil result is returned that is
;; validated, and ultimately returns the valid move.


;; update-game returns a new game based on a existing game plus a move,
;; and returns :draw, an player (:X or :O), or a set of valid next
;; moves and player to make the move.


(defn- new-board
  "Return new, empty, tic-tac-toe board"
  []
  (vec (map (fn [x] constantly nil) (board-indices))))

(defn new-game
  "Return new tic-tac-toe game with empty board and :X as next player to move."
  []
  (Game. nil :X (new-board)))

(defn game-after-move
  "Return new game object after applying a specific move"
  [g m]
  (let [b (assoc (:board g) (dec m) (:next-player g))
        p (if (= :X (:next-player g)) :O :X)
        s (game/status g)
        new-g (Game. s p b)]
    (if (> *verbose* 0)
      (println new-g))
    new-g))

(declare start-game!)

(defn read-next-move
  ""
  [g]
  (or (read-move-interactive g valid-move?)
      (do
        (println "Invalid move, try again.")
        (recur g))))

(defn next-move
  "Get next move, apply to game, recurse on result"
  [g]
  (txtui/print-game-status g)
  (let [m (read-next-move g)]
    (condp = m
      :resign (exit 0 "Resigning")
      :quit (exit 0 "Quitting")
      :start (start-game! nil)
      nil g
      (next-move (game-after-move g m)))))

(defn start-game!
  ""
  [options]
  (let [g (new-game)]
    (next-move g)))

(defn main
  "Use this in a REPL -- it won't exit, and it'll parse the args"
  [args]
  (comment (start-game! nil))
  (let [{:keys [action options exit-message ok?]} (validate-args
                                                   (if (string? args)
                                                     (string/split args #" ")
                                                     args))]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "start"  (binding [*verbose* (:verbose options)]
                   (start-game! options))))))

(defn -main
  "The formal main entrypoint"
  [& args]
  (let [res (main args)]
    (if-let [rc (:exit res)]
      (System/exit rc)
      res)))
