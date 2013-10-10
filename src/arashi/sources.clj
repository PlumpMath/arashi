(ns arashi.sources
  (:require [net.cgrand.enlive-html :as html]))

(defn fetch-html [url]
  (html/html-resource (java.net.URL. url)))

(defn hackernews []
  (let [frontpage-html (fetch-html "https://news.ycombinator.com")
        titles (html/select frontpage-html [:td.title :a])
        users-and-comments (html/select frontpage-html [:td.subtext :a])]
    (map (fn [title [user comments]]
           {:url (get-in title [:attrs :href])
            :title (html/text title)
            :author (str "https://news.ycombinator.com/" (get-in user [:attrs :href]))
            :comments (str "https://news.ycombinator.com/" (get-in comments [:attrs :href]))})
         titles (partition 2 users-and-comments))))

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
