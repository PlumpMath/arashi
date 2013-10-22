(ns lookalike.server
  (:use compojure.core
        [ring.middleware.params :only (wrap-params)]
        [ring.util.response :only (redirect response content-type)])
  (:require [clojure.pprint :as pp]
            [clojure.core.memoize :as memo])

  (:use [lookalike.cache :only (favicon-of)]))

(defn pp-str [obj]
  (let [w (java.io.StringWriter.)]
    (pp/pprint obj w)
    (.toString w)))

(defroutes app-routes
  (GET "/favicon" {{url "url"} :params}
       (redirect (favicon-of url)))
  (GET "/favicon_url" {{url "url"} :params}
       (-> (response (favicon-of url))
           (content-type "text/plain")))
  (GET "/cache" []
       (let [cache (memo/snapshot favicon-of)]
         (-> (response
              (pp-str {:n (count cache)
                       :cache cache}))
             (content-type "text/plain")))))

(def app
  (-> app-routes
      wrap-params))