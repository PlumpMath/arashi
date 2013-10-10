(ns arashi.sources
  (:require [net.cgrand.enlive-html :as html]))

(defn hackernews []
  [] #_(json/parse (http/get "http://api.ihackernews.com/page?format=json")))

(defn fetch-html [url]
  (html/html-resource (java.net.URL. url)))

(defn twitter [username]
  (let [user-html (fetch-html (str "https://mobile.twitter.com/" username))
        tweets (html/select user-html [:.tweet])
        contents (map html/text (html/select tweets [:.tweet-text]))
        users (map #(.trim (html/text %)) (html/select tweets [:.username]))
        urls (map #(str "https://twitter.com/" (first (html/attr-values % :href))) tweets)]
    (map (fn [content user url]
           {:url url
            :author user
            :title content})
         contents users urls)))

(twitter "FrauSchnatalie")
