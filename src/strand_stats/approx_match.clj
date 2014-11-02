(ns strand_stats.approx_match
  (:require bia_utils.util)
  (:require [clojure.math.combinatorics :as combo])
  )

(def ALPHABET "ACGT")

(defn replace-one-char [pattern pos char]
  (let [res-seq (assoc pattern pos char)]
    (apply str res-seq)))

(defn fill-one-position [pos pattern]
  (map #(replace-one-char pattern pos %) ALPHABET))

(defn build-one-approx-pattern [positions pattern]
  (let [complemented (map #(fill-one-position % pattern) positions)]
    (flatten complemented)))

(defn build-approx-patterns [d pattern]
  (let [pattern-seq (vec pattern)
        len (count pattern)
        combs (combo/combinations (range len) d)
        patterns (map #(build-one-approx-pattern % pattern-seq) combs)]
    (into #{} patterns)))


(defn find-approx-match [d pattern text]
  (let [approx-patterns (build-approx-patterns d pattern)
        ]
    )
  )

