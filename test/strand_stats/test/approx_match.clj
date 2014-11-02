(ns strand_stats.test.approx_match
  (:use [strand_stats.approx_match])
  (:use clojure.tools.trace)
  (:use [clojure.test]))

;; (trace-ns 'strand_stats.approx_match)

(deftest replace-one-char-test
  (is (= "AGGAC"
         (strand_stats.approx_match/replace-one-char (vec "CGGAC") 0 \A)))
  (is (= "CGGAA"
         (strand_stats.approx_match/replace-one-char (vec "CGGAC") 4 \A)))
  )

