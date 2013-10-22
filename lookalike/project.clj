(defproject lookalike "0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.7.9"]
                 [org.clojure/core.memoize "0.5.6"]

                 [compojure "1.1.5"]
                 [ring "1.2.0"]

                 [enlive "1.1.4"]]
  :plugins [[lein-ring "0.8.7"]]
  :ring {:handler lookalike.server/app})
