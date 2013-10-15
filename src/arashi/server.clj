(ns arashi.server
  (:use ring.adapter.jetty)
  (:use compojure.core)

  (:use hiccup.core)

  (:require [arashi.posts :as posts]
            [arashi.sources :as src]
            [arashi.background :as bg]
            [arashi.render :as r] :reload)

  (:import java.util.Date
           org.ocpsoft.prettytime.PrettyTime))

(def posts (ref (posts/posts-set)))

(def sources
  {:twitter ["fogus" "djspiewak" "bodil" "richhickey" "stuarthalloway"]})

(bg/fetch-posts posts (concat [src/hackernews] (map #(fn [] (src/twitter %)) (:twitter sources))))

(defroutes app
  (GET "/" []
       (apply str (r/posts-tmpl (reverse @posts))))
  (POST "/superfeedr" req
        (prn (-> req :body slurp))
        "ok"))

(defn run! []
  (run-jetty app {:port 1234}))
