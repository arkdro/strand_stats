(ns strand_stats.approx_match
  (:require bia_utils.util)
  (:require [clojure.math.combinatorics :as combo])
  )

(def ALPHABET "ACGT")

(defn replace-one-char [pattern pos char]
  (let [res-seq (assoc pattern pos char)]
    res-seq
    ))

(defn fill-one-position [pos pattern]
  (map #(replace-one-char pattern pos %) ALPHABET))

(defn build-one-approx-pattern [[head & tail] pattern]
  (if (nil? head) (apply str pattern)
      (let [head-updated (fill-one-position head pattern)
            res (map #(build-one-approx-pattern tail %)
                     head-updated)]
        (flatten res))))

(defn calc-combinations [d pattern]
  (let [len (count pattern)
        allowed-mismatches (range 1 (inc d))
        combs (mapcat #(combo/combinations (range len) %)
                      allowed-mismatches)]
    combs))

(defn build-approx-patterns [d pattern]
  (let [pattern-seq (vec pattern) ;; convert to vector once, at the beginning
        combs (calc-combinations d pattern)
        patterns (mapcat #(build-one-approx-pattern % pattern-seq) combs)]
    (into #{} patterns)))


(defn find-approx-match [d pattern text]
  (let [approx-patterns (build-approx-patterns d pattern)
        ]
    )
  )

