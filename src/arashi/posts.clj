(ns arashi.posts)

; post: url + title
;  check whether it's a new one (url unknown, doesn't work for HN)
;  sort by timestamp for display

(defn new? [posts post]
  (not-any? #(= (:url post) (:url %)) posts))

(defn join [old new] ; old could just be a (sorted-)set
  (apply conj old (filter #(new? old %) new)))

(defn posts-set []
  (sorted-set-by #(compare (:timestamp %1) (:timestamp %2))))