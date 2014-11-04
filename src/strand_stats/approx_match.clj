(ns strand_stats.approx_match
  (:require clojure.set)
  (:require bia_utils.reverse_complement)
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

(defn build-approx-patterns-two-way
  "Build approximate patterns for k-mer and its reverse complement"
  [d pattern]
  (let [forward-patterns (build-approx-patterns d pattern)
        revc-patterns (build-approx-patterns
                       d (bia_utils.reverse_complement/rev-complement pattern))]
    (clojure.set/union forward-patterns revc-patterns)))

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

(defn build-all-approx-patterns-aux [d [head & tail] acc]
  (if (nil? head) acc
      (let [patterns (build-approx-patterns d head)
            new-acc (assoc acc head patterns)]
        (recur d tail new-acc))))

(defn build-all-approx-patterns
  "Build approximate patterns for every k-mer from the list"
  [d k-mers]
  (build-all-approx-patterns-aux d k-mers {}))

(defn build-all-patterns-two-way-aux [d [head & tail] acc]
  (if (nil? head) acc
      (let [patterns (build-approx-patterns-two-way d head)
            new-acc (assoc acc head patterns)]
        (recur d tail new-acc))))

(defn build-all-patterns-two-way
  "Build approximate patterns for every k-mer (and its implied reverse
   complement) from the list"
  [d k-mers]
  (build-all-patterns-two-way-aux d k-mers {}))

(defn find-freq-approx-match-aux [k d text [k-mer & k-mers-tail]
                                  all-approx-patterns acc]
  (if (nil? k-mer) acc
      (let [approx-patterns (get all-approx-patterns k-mer)
            positions (iter-over-text k-mer approx-patterns text)
            n (count positions)
            new-acc (assoc acc k-mer n)]
        (recur k d text k-mers-tail all-approx-patterns new-acc))))

(defn find-freq-approx-match
  "Find the frequencies of k-mers with mismatches in a string"
  [k d text]
  (let [k-mers (find-all-k-mers k text)
        all-approx-patterns (build-all-approx-patterns d k-mers)
        res (find-freq-approx-match-aux k d text k-mers all-approx-patterns {})]
    res))

(defn increase-every-neighbour [k-mer approx-patterns acc]
  (let [neighbours (get approx-patterns k-mer)
        new-acc (reduce #(bia_utils.util/add-provided-k-mer %2 %1)
                        acc
                        neighbours)]
    new-acc))

(defn find-freq-approx-match2-aux [idx len k d text approx-patterns acc]
  (if (bia_utils.util/is-data-available idx len k)
    (let [current-k-mer (bia_utils.util/get-one-k-mer idx text k)
          new-acc (increase-every-neighbour current-k-mer approx-patterns acc)]
      (recur (inc idx) len k d text approx-patterns new-acc))
    acc))

(defn find-freq-approx-match2
  "Find the frequencies of k-mers with mismatches in a string. Ver. 2"
  [k d text]
  (let [k-mers (find-all-k-mers k text)
        all-approx-patterns (build-all-approx-patterns d k-mers)
        res (find-freq-approx-match2-aux 0 (count text) k d text
                                         all-approx-patterns {})]
    res))

(defn most-freq-approx-match2 [k d text]
  (let [freqs (find-freq-approx-match2 k d text)
        max-items (bia_utils.util/find-max-items (into [] freqs))
        k-mers (map first max-items)]
    (sort k-mers)))

(defn most-freq-approx-match [k d text]
  (let [freqs (find-freq-approx-match k d text)
        max-items (bia_utils.util/find-max-items (into [] freqs))
        k-mers (map first max-items)]
    (sort k-mers)))

(defn find-freq-match-revc
  "Find the frequencies of k-mers with mismatches and reverse complements"
  [k d text]
  (let [
        ;; revc-text (bia_utils.reverse_complement/rev-complement text)
        k-mers (find-all-k-mers k text)
        all-patterns (build-all-patterns-two-way d k-mers)
        res (find-freq-approx-match2-aux 0 (count text) k d text
                                         all-patterns {})
        ]
    res)
  )
