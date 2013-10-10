(ns arashi.sources
  (:require [net.cgrand.enlive-html :as html]))

(defn fetch-html [url]
  (html/html-resource (java.net.URL. url)))

(defn parse-date [date-string]
  (let [dates (.parse (com.joestelmach.natty.Parser.) date-string)]
    (if (empty? dates)
      nil
      (-> dates first .getDates first))))

(defn hackernews []
  (let [frontpage-html (fetch-html "https://news.ycombinator.com")
        titles (html/select frontpage-html [:td.title :a])
        more-infos (html/select frontpage-html [:td.subtext])
        times-ago (html/select more-infos [(html/text-pred #(.contains % "ago"))])
        users-and-comments (html/select more-infos [:a])]
    (map (fn [title [user comments] time-ago]
           {:url (get-in title [:attrs :href])
            :title (html/text title)
            :timestamp (parse-date time-ago)
            :author (str "https://news.ycombinator.com/" (get-in user [:attrs :href]))
            :comments (str "https://news.ycombinator.com/" (get-in comments [:attrs :href]))})
         titles (partition 2 users-and-comments) times-ago)))

(defn hackernews-post? [post]
  (.contains (:comments post) "news.ycombinator.com/"))

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

(defn twitter-post? [post]
  (.contains (:url post) "twitter.com/"))
