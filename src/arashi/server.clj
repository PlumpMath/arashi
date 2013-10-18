(ns arashi.server
  (:use ring.adapter.jetty
        [ring.middleware.params :only (wrap-params)])
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
   {:hackernews ["hn"]
    :twitter ["fogus" "djspiewak" "bodil" "richhickey" "stuarthalloway"]
    :feed ["http://prog21.dadgum.com" "https://github.com/blog/all.atom" "http://codinghorror.com"
           "http://waxy.org/links"]}})

(defn read-config []
  (if (.exists (java.io.File. "config.edn"))
    (-> "config.edn" slurp clojure.tools.reader.edn/read-string)
    default-config))

(defn expand-sources [sources]
  (if (map? sources)
    (mapcat (fn [[k v]]
              (map #(array-map :source k (src/source-key k) %) v))
            sources)
    sources))

(def sources
  (-> (read-config) :sources expand-sources))

(defonce posts (ref (posts/posts-set)))

(defonce bg-fetching
  (bg/fetch-posts posts (map src/fetch-from sources)))

(defn pretty-agent [a]
  (if-let [err (agent-error a)]
    err
    a))

(defn pp-str [obj]
  (let [w (java.io.StringWriter.)]
    (clojure.pprint/pprint obj w)
    (.toString w)))

(defroutes app-routes
  (GET "/" []
       (apply str (r/posts-tmpl (reverse @posts))))
  (GET "/status" []
       (-> (resp/response
            (reverse (sort-by (comp :last-error-t deref) @bg-fetching)))
           (resp/content-type "text/plain")))
  (GET "/posts.edn" []
       (-> (resp/response (reverse @posts))
           (resp/content-type "text/plain")))
  (POST "/superfeedr" req
        (prn (-> req :body slurp))
        "ok"))

(defn pretty-print-edn [handler]
  (fn [req]
    (let [{{pretty "pretty"} :query-params} req
          resp (handler req)
          not-string? (not (instance? String (:body resp)))]
      (if not-string?
        (if (= pretty "true")
          (update-in resp [:body] pp-str)
          (update-in resp [:body] prn-str))
        resp))))

(def app
  (-> app-routes
      pretty-print-edn
      wrap-params))

(defn run! []
  (run-jetty app {:port 1234}))
