(ns arashi.render
  (:use [net.cgrand.enlive-html :only [deftemplate defsnippet html]])
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html])
  (:import org.ocpsoft.prettytime.PrettyTime
           java.util.Date))

(defn root-url [url]
  (let [url (java.net.URL. url)]
    (str (.getProtocol url) "://" (.getHost url))))

(defsnippet post-tmpl "arashi/posts.html" [:#posts [:.post (html/nth-of-type 1)]]
  [{:keys [title url via timestamp]}]
  html/this-node (html/set-attr :class (str "post" " " (str/replace (.getHost (java.net.URL. (or via url))) #"\." "-")))
  [:a] (html/do->
        (html/content title)
        (html/set-attr :href url))
  [:.timestamp] (html/content  (str "(" (.format (PrettyTime.) (or timestamp (Date.))) ")"))
  [:.icon] (html/set-attr :src (str "/lookalike/favicon?url=" (root-url url)))
  [:.via :a] (html/do->
              (html/content (-> (or via url) (java.net.URL.) .getHost))
              (html/set-attr :href (or via url))))

(deftemplate posts-tmpl "arashi/posts.html"
  [posts]
  [:#posts] (html/content (map post-tmpl posts)))