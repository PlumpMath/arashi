(ns arashi.server
  (:use ring.adapter.jetty)
  (:use compojure.core))

(defroutes app
  (POST "/superfeedr" req
        (prn req)
        "ok"))

(defn run! []
  (run-jetty app {:port 1234}))
