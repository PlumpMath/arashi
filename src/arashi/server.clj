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

(def default-config
  {:sources
   {:twitter ["fogus" "djspiewak" "bodil" "richhickey" "stuarthalloway"]
    :feed ["http://prog21.dadgum.com" "https://github.com/blog/all.atom" "http://codinghorror.com"
           "http://waxy.org/links"]}})

(defn read-config []
  (if (.exists (java.io.File. "config.edn"))
    (-> "config.edn" slurp clojure.tools.reader.edn/read-string)
    default-config))

(def sources
  (:sources (read-config)))

(defonce bg-fetching
  (bg/fetch-posts posts (concat [src/hackernews]
                                (map #(fn [] (src/twitter %)) (:twitter sources))
                                (map #(fn [] (src/feed %)) (:feed sources)))))

(defroutes app
  (GET "/" []
       (apply str (r/posts-tmpl (reverse @posts))))
  (POST "/superfeedr" req
        (prn (-> req :body slurp))
        "ok"))

(defn run! []
  (run-jetty app {:port 1234}))
