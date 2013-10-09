(defproject arashi "0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]

                 ; web server
                 [compojure "1.1.5"]
                 [ring "1.2.0"]]
  :plugins [[lein-ring "0.8.7"]]
  :ring {:handler arashi.server/app})