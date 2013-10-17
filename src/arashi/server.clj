(ns arashi.server
  (:use ring.adapter.jetty)
  (:use compojure.core)
  (:require [ring.util.response :as resp])

  (:use hiccup.core)

  (:require [arashi.posts :as posts]
            [arashi.sources :as src]
            [arashi.background :as bg]
            [arashi.render :as r] :reload)

  (:import java.util.Date
           org.ocpsoft.prettytime.PrettyTime))

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

(defonce posts (ref (posts/posts-set)))

(defonce bg-fetching
  (bg/fetch-posts posts (concat [src/hackernews src/pg-essays]
                                (map #(fn [] (src/twitter %)) (:twitter sources))
                                (map #(fn [] (src/feed %)) (:feed sources)))))

(defn pretty-agent [a]
  (if-let [err (agent-error a)]
    err
    a))

(defroutes app
  (GET "/" []
       (apply str (r/posts-tmpl (reverse @posts))))
  (GET "/status" []
       (-> (resp/response (prn-str (map pretty-agent @bg-fetching)))
           (resp/content-type "text/plain")))
  (POST "/superfeedr" req
        (prn (-> req :body slurp))
        "ok"))

(defn run! []
  (run-jetty app {:port 1234}))
