(ns strand_stats.test.hamming
  (:use [strand_stats.hamming])
  (:use clojure.tools.trace)
  (:use [clojure.test]))

;; (trace-ns 'strand_stats.skew)

(deftest hamming-distance-test
  (is (= 2 (strand_stats.hamming/hamming-distance "CGAAT" "CGGAC")))
  (is (= 3 (strand_stats.hamming/hamming-distance "GGGCCGTTGGT" "GGACCGTTGAC")))
  )

