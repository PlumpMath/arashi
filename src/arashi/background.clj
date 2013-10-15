(ns arashi.background
  "Fetches posts from various sources in a background thread.

Uses a backoff to check non-frequently updated sources less often."
  (:require [arashi.posts :as p]
            [arashi.sources :as s]))

(def min-interval (* 1 60))
(def max-interval (* 1 24 60 60))

(defn backoff-inc [n]
  (min (* n 2) max-interval))

(defn backoff-dec [n]
  (max (/ n 2) min-interval))

(defn update-posts [posts-ref new-posts]
  (dosync
   (let [posts @posts-ref
         updated-posts (p/join posts new-posts)]
     (ref-set posts-ref updated-posts)

     (if (= (count posts) (count updated-posts))
       backoff-inc
       backoff-dec))))

(defn backoff-fn [a f]
  (fn backoff-fn* [interval]
    (let [change-fn (f)]
      (send-off a backoff-fn*)
      (println interval)
      (Thread/sleep (* 1000 interval))
      (change-fn interval))))


(defn fetch-posts [posts fetch-fns]
  (doseq [fetch-fn fetch-fns]
    (future
      (let [a (agent min-interval)]
        (send-off a (backoff-fn a #(update-posts posts (fetch-fn))))))))

(defn example []
  (def ps (ref (p/posts-set)))
  (fetch-posts ps [#(s/twitter "fogus") #(s/twitter "djspiewak") s/hackernews])

  (map :title @ps))

;(example)
;(doall (map println (map :title @ps)))
