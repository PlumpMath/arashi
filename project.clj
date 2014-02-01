(defproject arashi "0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]

                 ; web server
                 [compojure "1.1.5"]
                 [ring "1.2.0"]
                 [hiccup "1.0.4"]
                 [org.ocpsoft.prettytime/prettytime "3.1.0.Final"]
                 [lookalike "0.0-SNAPSHOT"]

                 ; scraping
                 ; `mvn install` locally from github.com/rometools/rome: [com.rometools/rome "???"]
                 [clj-http "0.7.7"]
                 [org.clojars.scsibug/feedparser-clj "0.4.0"]
                 [enlive "1.1.4"]
                 [com.rubiconproject.oss/jchronic "0.2.6"]]
  :plugins [[lein-ring "0.8.7"]]
  :ring {:init arashi.server/init-fetchers
         :handler arashi.server/app
         :nrepl {:start? true
                 :port 30583}})
