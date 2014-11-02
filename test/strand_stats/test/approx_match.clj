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

(deftest fill-one-position-test
  (is (= '("CAGAC"
           "CCGAC"
           "CGGAC"
           "CTGAC")
         (strand_stats.approx_match/fill-one-position 1 (vec "CGGAC"))))
  (is (= '("CGGAA"
           "CGGAC"
           "CGGAG"
           "CGGAT")
         (strand_stats.approx_match/fill-one-position 4 (vec "CGGAC"))))
  )

(deftest build-one-approx-pattern-test
  (is (= ["CAGAC" "CCGAC" "CGGAC" "CTGAC"
          "CGGAA" "CGGAC" "CGGAG" "CGGAT"]
         (strand_stats.approx_match/build-one-approx-pattern
          [1 4] (vec "CGGAC"))))
  )

(deftest calc-combinations-test
  (is (= [[0] [1] [2] [3] [0 1] [0 2] [0 3] [1 2] [1 3] [2 3]]
         (strand_stats.approx_match/calc-combinations 2 "CAGA")))
  )

