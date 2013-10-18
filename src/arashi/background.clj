(ns arashi.background
  "Fetches posts from various sources in a background thread.

Uses a backoff to check non-frequently updated sources less often."
  (:require [arashi.posts :as p]
            [arashi.sources :as s]))

(def min-interval (* 1 60))
(def max-interval (* 1 24 60 60))

(defn backoff-inc [n]
  (min (* n 2) max-interval))

(def backoff-reset
  (constantly min-interval))

(defn update-posts [posts-ref new-posts]
  (dosync
   (let [posts @posts-ref
         updated-posts (p/join posts new-posts)]
     (ref-set posts-ref updated-posts)

     (if (= (map :title posts) (map :title updated-posts))
       backoff-inc
       backoff-reset))))

(defn backoff-fn [a f] ; maybe use timing thread (dates, not intervals) that spawns tasks?
  (fn backoff-fn* [{:keys [interval] :as fetch-state}]
    (send-off a backoff-fn*)
    (let [change-fn (f)]
      (println interval (str f) (str change-fn))
      (Thread/sleep (* 1000 interval))
      (assoc (dissoc fetch-state :error :last-error-t)
        :interval (change-fn interval)
        :last-fetch-t (java.util.Date.)))))

(defn store-error [a error]
  (send a assoc
        :error error
        :last-error-t (java.util.Date.)))

(defn fetch-posts [posts fetch-infos]
  (let [as (atom [])]
    (doseq [{:keys [fetch-fn] :as fetch-info} fetch-infos]
      (let [a (agent (assoc fetch-info :interval min-interval)
                     :error-mode :continue
                     :error-handler store-error)]
        (send-off a (backoff-fn a #(update-posts posts (fetch-fn))))
        (swap! as conj a)))
    as))

(defn example []
  (def ps (ref (p/posts-set)))
  (fetch-posts ps [#(s/twitter "fogus") #(s/twitter "djspiewak") s/hackernews])

  (map :title @ps))

;(example)
;(doall (map println (map :title @ps)))
