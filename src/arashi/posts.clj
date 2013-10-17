(ns arashi.posts)

; post: url + title
;  check whether it's a new one (url unknown, doesn't work for HN)
;  sort by timestamp for display

(defn new? [posts post]
  (not-any? #(= (:url post) (:url %)) posts))

(defn join [old new] ; old could just be a (sorted-)set
  (let [new (filter #(new? old %) new)]
    (if (empty? new)
      old
      (apply conj old new))))

(defn posts-set []
  (sorted-set-by (fn [p1 p2]
                   (let [tc (compare (:timestamp p1) (:timestamp p2))]
                     (if (= tc 0)
                       (compare (:url p1) (:url p2))
                       tc)))))
