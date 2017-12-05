(ns tic-tac-toe.core
  (:require
   [clojure.core.reducers :as reducers]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [tic-tac-toe.game :as game]
   [tic-tac-toe.txtui :as txtui]
   )
  (:gen-class))

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

(defprotocol Player
  "Define interface for fns that implement (or talk with) a player"
  ;; Choose next move for the player, based on Game.
  (choose [this])
  ;; Report state of Game to player. 'why' is :move, :retry, :win,
  ;; :lose, or :draw; 'what' is nil or { <player-kw> <move> }; 'valid'
  ;; is set of valid moves (1-9).
  (report [this why what valid]))

(declare start-game!)

(defn read-next-move
  ""
  [g prompt]
  (or (do
        (print prompt)
        (flush)
        (txtui/read-move-interactive g game/valid-move?))
      (recur g prompt)))

(defn next-move
  "Get next move, apply to game, recurse on result"
  [g]
  (let [m (read-next-move
           g
           (txtui/print-game-status g))]
    (condp = m
      :resign (exit 0 "\nResigning.\n")
      :quit (exit 0 "\nQuitting.\n")
      :start (start-game! nil)
      :back (do
              (println "\nBacking up one move.\n")
              :back)
      :reset (do
               (println "\nResetting game.\n")
               :reset)
      :help (recur g)
      nil g  ; This shouldn't happen anymore.
      (let [n (next-move (game/after-move g m))]
            (condp = n
              :back (recur g)
              n)))))

(defn start-game!
  ""
  [options]
  (let [g (game/new)
        n (next-move g)]
    (condp = n
      :reset (recur options)
      :back (do
              (println "(Can't go back to previous game, so just resetting.)\n")
              (recur options))
      n)))

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
        "start"  (binding [game/*verbose* (:verbose options)]
                   (start-game! options))))))

(defn -main
  "The formal main entrypoint"
  [& args]
  (let [res (main args)]
    (if-let [rc (:exit res)]
      (System/exit rc)
      res)))
