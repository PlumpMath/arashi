(ns arashi.render
  (:use [net.cgrand.enlive-html :only [deftemplate defsnippet html]])
  (:require [net.cgrand.enlive-html :as html])
  (:import org.ocpsoft.prettytime.PrettyTime
           java.util.Date))

(defn root-url [url]
  (let [url (java.net.URL. url)]
    (str (.getProtocol url) "://" (.getHost url))))

(defsnippet post-tmpl "arashi/posts.html" [:#posts [:.post (html/nth-of-type 1)]]
  [{:keys [title url timestamp]}]
  [:a] (html/do->
        (html/content title)
        (html/set-attr :href url))
  [:.timestamp] (html/content  (str "(" (.format (PrettyTime.) (or timestamp (Date.))) ")"))
  [:.icon] (html/set-attr :src (str (root-url url) "/favicon.ico")))

(deftemplate posts-tmpl "arashi/posts.html"
  [posts]
  [:#posts] (html/content (map post-tmpl posts)))