(defproject tic-tac-toe "0.1.0-SNAPSHOT"
  :description "Classic simple game of tic-tac-toe"
  :url "https://github.com/jcburley/tic-tac-toe"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [org.clojure/tools.cli "0.3.5"]]
  :main ^:skip-aot tic-tac-toe.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns tic-tac-toe.core})
