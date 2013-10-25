(ns lookalike.core
  "Find all the favicons as lazy as possible."
  (:require [net.cgrand.enlive-html :as html]
            [clojure.tools.reader.edn :as edn]))

(defn select-favicon [html]
  (html/select html [[:link (html/attr-has :rel "icon")]]))

(defn resolve-url [base-url url]
  (let [base-uri (java.net.URI. base-url)
        uri (java.net.URI. url)]
    (str (if (.isAbsolute uri)
           uri
           (.resolve base-uri (if (.startsWith url "/")
                                url
                                (str "/" url)))))))

(defn root-url [url]
  (let [url (java.net.URL. url)]
    (str (.getProtocol url) "://" (.getHost url))))

(defn favicon-of [url]
  "Retrieve the favicon for a web page.

Tries to find `<link rel=\"icon\" href=\"...\">` links in the HTML of
the page. If that fails it returns `\"[scheme]://[host]/favicon.ico\"`.

E.g. `(favicon-of \"http://paulgraham.com\")` ;=> \"http://ycombinator.com/arc/arc.png\""
  (let [html (html/html-resource (java.net.URL. url))
        favicon (first (select-favicon html))]
    (resolve-url url
                 (or (get-in favicon [:attrs :href])
                     (str (root-url url) "/favicon.ico")))))
