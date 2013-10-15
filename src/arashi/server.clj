(ns arashi.server
  (:use ring.adapter.jetty)
  (:use compojure.core)

  (:use hiccup.core)

  (:require [arashi.posts :as posts]
            [arashi.sources :as src]
            [arashi.background :as bg])

  (:import java.util.Date
           org.ocpsoft.prettytime.PrettyTime))

(def posts (ref (posts/posts-set)))

(def sources
  {:twitter ["fogus" "djspiewak" "bodil" "richhickey" "stuarthalloway"]})

(bg/fetch-posts posts (concat [src/hackernews] (map #(fn [] (src/twitter %)) (:twitter sources))))

(defn render-post [{:keys [title url timestamp]}]
  [:article.post
   [:a {:href url} [:h1 title]]
   [:span.timestamp "(" (.format (PrettyTime.) (or timestamp (Date.))) ")"]])

(defn list-posts [posts]
  (html (for [post posts]
          (render-post post))))

(defroutes app
  (GET "/" []
       (list-posts @posts))
  (POST "/superfeedr" req
        (prn (-> req :body slurp))
        "ok"))

(defn run! []
  (run-jetty app {:port 1234}))
