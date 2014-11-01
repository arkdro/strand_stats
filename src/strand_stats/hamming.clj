(ns strand_stats.hamming
  ;; (:require bia_utils.util)
  )

(defn hamming-distance-aux [cnt [h1 & tail1] [h2 & tail2]]
  (cond
    (nil? h1) cnt
    (nil? h2) cnt
    (= h1 h2) (recur cnt tail1 tail2)
    :default (recur (inc cnt) tail1 tail2)))

(defn hamming-distance [text1 text2]
  (hamming-distance-aux 0 text1 text2))

