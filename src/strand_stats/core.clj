(ns strand_stats.core
  {:doc "pattern count"}
  (:use clj-getopts.core)
  (:require strand_stats.skew)
  (:require strand_stats.hamming)
  (:require strand_stats.approx_match)
  (:require clojure.string)
  (:use clojure.tools.trace)
  (:gen-class)
  )

;;(trace-ns 'strand_stats.util)

(defn read-lines [fd skip]
  (let [
        lines (line-seq fd)
        lines2 (drop skip lines)
        [line1 line2] (take 2 lines2)
        res [line1 line2]
        ]
    res))

(defn read-file [fname skip]
  (let [acc (with-open [fd (clojure.java.io/reader fname)]
              (doall
               (read-lines fd skip)
               ))]
    acc))

(defn read-data [opts]
  (let [fname (get opts :infile)
        skip-str (get opts :skip "0")
        skip (Integer/parseInt skip-str)
        ]
    (read-file fname skip)))

(defn prepare-line [start stop line]
  (let [stop2 (min stop (count line))
        short-line (subs line start stop2)]
    (clojure.string/upper-case short-line)))

(defn skew [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :start :arg
                                     :stop :arg
                                     :skip :arg}) args)
        [line] (read-data opts)
        len (count line)
        start (Integer/parseInt (get opts :start "0"))
        stop (Integer/parseInt (String/valueOf (get opts :stop len)))
        prepared-line (prepare-line start stop line)
        res (strand_stats.skew/skew prepared-line)]
    res))

(defn skew-min [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :skip :arg}) args)
        [line] (read-data opts)
        res (strand_stats.skew/find-skew-min (clojure.string/upper-case line))]
    res))

(defn hamming-distance [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :skip :arg}) args)
        [line1 line2] (read-data opts)
        res (strand_stats.hamming/hamming-distance
             (clojure.string/upper-case line1)
             (clojure.string/upper-case line2))]
    res))

(defn approx-match [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :d :arg
                                     :skip :arg}) args)
        d (Integer/parseInt (get opts :d "0"))
        [pattern text] (read-data opts)
        res (strand_stats.approx_match/find-approx-match
             d
             (clojure.string/upper-case pattern)
             (clojure.string/upper-case text))]
    res))

(defn approx-match-count [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :d :arg
                                     :skip :arg}) args)
        d (Integer/parseInt (get opts :d "0"))
        [pattern text] (read-data opts)
        res (strand_stats.approx_match/count-approx-match
             d
             (clojure.string/upper-case pattern)
             (clojure.string/upper-case text))]
    res))

(defn freq-approx-match [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :d :arg
                                     :k :arg
                                     :skip :arg}) args)
        [text] (read-data opts)
        len (count text)
        k (Integer/parseInt (String/valueOf (get opts :k len)))
        d (Integer/parseInt (get opts :d "0"))
        res (strand_stats.approx_match/find-freq-approx-match2
             k d (clojure.string/upper-case text))]
    res))

(defn most-freq-approx-match [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :d :arg
                                     :k :arg
                                     :skip :arg}) args)
        [text] (read-data opts)
        len (count text)
        k (Integer/parseInt (String/valueOf (get opts :k len)))
        d (Integer/parseInt (get opts :d "0"))
        res (strand_stats.approx_match/most-freq-approx-match2
             k d (clojure.string/upper-case text))]
    res))

(defn freq-match-revc [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :d :arg
                                     :k :arg
                                     :skip :arg}) args)
        [text] (read-data opts)
        len (count text)
        k (Integer/parseInt (String/valueOf (get opts :k len)))
        d (Integer/parseInt (get opts :d "0"))
        res (strand_stats.approx_match/find-freq-match-revc
             k d (clojure.string/upper-case text))]
    res))

(defn -main [& args]
  (let [opts (getopts (options "is" {:fun :arg}) args)]
    (case (get opts :fun)
      "skew" (skew args)
      "skew-min" (skew-min args)
      "h-dist" (hamming-distance args)
      "approx-match" (approx-match args)
      "approx-match-count" (approx-match-count args)
      "freq-approx-match" (freq-approx-match args)
      "most-freq-approx-match" (most-freq-approx-match args)
      "freq-match-revc" (freq-match-revc args)
      )
    ))

