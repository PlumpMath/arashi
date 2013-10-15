(ns arashi.sources
  "Datasources for various services that don't provide RSS/ATOM feeds.

Currently, HackerNews and Twitter are supported."
  (:require [net.cgrand.enlive-html :as html])

  (:import java.util.Calendar))

(defn fetch-html [url]
  (html/html-resource (java.net.URL. url)))

(defn parse-date [date-string]
  (let [dates (com.mdimension.jchronic.Chronic/parse date-string)]
    (if (nil? dates)
      nil
      (java.util.Date. (* 1000 (.getBegin dates))))))

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

(defn parse-date-with [format date]
  (try
    (.parse (java.text.SimpleDateFormat. format) date)
    (catch java.text.ParseException e
      nil)))

(defn parse-twitter-date [date]
  (let [short-date (re-find #"(\d+)(m|h|d)" date)]
    (if short-date
      (let [[_ n suffix] short-date
            expanded-suffix (condp = suffix
                              "m" "minutes"
                              "h" "hours"
                              "d" "days")]
        (parse-date (str n " " expanded-suffix " ago")))
      (or (parse-date-with "dd MMM yy" date)
          (parse-date-with "dd MMM yy" (str date " " (-> (Calendar/getInstance) (.get Calendar/YEAR) .toString (subs 2))))))))

(defn twitter [username]
  (let [user-html (fetch-html (str "https://mobile.twitter.com/" username))
        tweets (html/select user-html [:.tweet])
        contents (map html/text (html/select tweets [:.tweet-text]))
        users (map #(.trim (html/text %)) (html/select tweets [:.username]))
        timestamps (map html/text (html/select tweets [:.timestamp]))
        urls (map #(str "https://twitter.com/" (first (html/attr-values % :href))) tweets)]
    (map (fn [content user url timestamp]
           {:url url
            :author user
            :timestamp (parse-twitter-date (.trim timestamp))
            :title content})
         contents users urls timestamps)))

(defn twitter-post? [post]
  (.contains (:url post) "twitter.com/"))
