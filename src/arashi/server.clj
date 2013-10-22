(ns arashi.server
  (:use ring.adapter.jetty
        [ring.middleware.params :only (wrap-params)])
  (:use compojure.core)
  (:require [ring.util.response :as resp])

  (:use hiccup.core)

  (:require [lookalike.server :as lookalike])
  (:require [arashi.posts :as posts]
            [arashi.sources :as src]
            [arashi.background :as bg]
            [arashi.render :as r] :reload)

  (:import java.util.Date
           org.ocpsoft.prettytime.PrettyTime))

(defn read-edn [filename]
  (-> filename slurp clojure.tools.reader.edn/read-string))

(defn read-config []
  (let [config-file (or (System/getProperty "arashi.config.path") "config.edn")]
    (read-edn config-file)))

(defn expand-sources [sources]
  (if (map? sources)
    (mapcat (fn [[k v]]
              (map #(array-map :source k (src/source-key k) %) v))
            sources)
    sources))

(def sources
  (-> (read-config) :sources expand-sources))

(defonce posts
  (ref (posts/join (posts/posts-set)
                   (if (.exists (java.io.File. "all_posts.edn"))
                     (read-edn "all_posts.edn")
                     []))))

(defn save-posts-agent []
  (let [a (agent {:type :save-posts
                  :interval (* 10 60)})]
    (send-off a (bg/backoff-fn a (fn []
                                   (with-open [f (clojure.java.io/writer "all_posts.edn")]
                                     (.write f (prn-str @posts)))
                                   identity)))
    a))

(defonce bg-fetching
  (let [bg-fetchers (bg/fetch-posts posts (map src/fetch-from sources))]
    (swap! bg-fetchers conj (save-posts-agent))
    bg-fetchers))

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
        "ok")
  (context "/lookalike" []
           lookalike/app-routes))

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
