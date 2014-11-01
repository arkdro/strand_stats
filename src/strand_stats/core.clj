(ns strand_stats.core
  {:doc "pattern count"}
  (:use clj-getopts.core)
  (:require strand_stats.skew)
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

(defn skew [args]
  (let [opts (getopts (options "is" {:infile :arg
                                     :start :arg
                                     :stop :arg
                                     :skip :arg}) args)
        [line] (read-data opts)
        len (count line)
        start (Integer/parseInt (get opts :start "0"))
        stop (Integer/parseInt (String/valueOf (get opts :stop len)))
        res (strand_stats.skew/skew start stop line)]
    res))

(defn -main [& args]
  (let [opts (getopts (options "is" {:fun :arg}) args)]
    (case (get opts :fun)
      "skew" (skew args)
      )
    ))

