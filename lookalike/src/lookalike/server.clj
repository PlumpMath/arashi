(ns lookalike.server
  (:use compojure.core
        [ring.middleware.params :only (wrap-params)]
        [ring.util.response :only (redirect response content-type)])

  (:use [lookalike.cache :only (favicon-of)]))

(defroutes app-routes
  (GET "/favicon" {{url "url"} :params}
       (redirect (favicon-of url)))
  (GET "/favicon_url" {{url "url"} :params}
       (-> (response (favicon-of url))
           (content-type "text/plain"))))

(def app
  (-> app-routes
      wrap-params))