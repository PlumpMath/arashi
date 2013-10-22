(ns lookalike.cache
  "Provides a caching version of `lookalike.core/favicon-of`."
  (:require [clojure.core.memoize :as memo]
            [lookalike.core :as lookalike]))

(def favicon-of (memo/lru lookalike/favicon-of :lru/threshold 1000))