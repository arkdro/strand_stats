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

(defn approx-matched [pattern patterns]
  (contains? patterns pattern))

(defn add-one-pattern [idx k acc base-pattern patterns text]
  (let [current-k-mer (bia_utils.util/get-one-k-mer idx text k)]
    (if (approx-matched current-k-mer patterns)
      (bia_utils.util/add-provided-k-mer base-pattern acc)
      (bia_utils.util/add-provided-k-mer current-k-mer acc))))

(defn add-pos [idx acc]
  (conj acc idx))

(defn add-one-pattern-pos [idx k acc patterns text]
  (let [current-k-mer (bia_utils.util/get-one-k-mer idx text k)]
    (if (approx-matched current-k-mer patterns)
      (add-pos idx acc)
      acc)))

(defn iter-over-text-aux [idx k acc base-pattern patterns len text]
  (if (bia_utils.util/is-data-available idx len k)
    (recur (inc idx) k
           (add-one-pattern-pos idx k acc patterns text)
           base-pattern patterns len text)
    acc))

(defn iter-over-text [pattern approx-patterns text]
  (let [idx 0
        k (count pattern)
        acc []
        len (count text)]
    (iter-over-text-aux idx k acc pattern approx-patterns len text)))

(defn find-approx-match [d pattern text]
  (let [approx-patterns0 (build-approx-patterns d pattern)
        approx-patterns (conj approx-patterns0 pattern)
        res (iter-over-text pattern approx-patterns text)]
    res))

(defn count-approx-match [d pattern text]
  (let [lst (find-approx-match d pattern text)]
    (count lst)))

(defn add-one-k-mer-by-index
  "Extract k-mer from text, store k-mer into accumulator"
  [idx k text acc]
  (let [end (+ idx k)
        k-mer (subs text idx end)]
    (conj acc k-mer)))

(defn find-all-k-mers-aux
  "Slide the window over the text, extract k-mers,
   store them into the accumulator"
  [idx acc k len text]
  (if (bia_utils.util/is-data-available idx len k)
    (recur (inc idx)
           (add-one-k-mer-by-index idx k text acc)
           k len text)
    acc))

(defn find-all-k-mers
  "Find all k-mers of length k in the text"
  [k text]
  (let [k-mers (find-all-k-mers-aux 0 #{} k (count text) text)]
    (into [] k-mers)))

