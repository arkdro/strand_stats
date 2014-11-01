(ns strand_stats.test.skew
  (:use [strand_stats.skew])
  (:use clojure.tools.trace)
  (:use [clojure.test]))

;; (trace-ns 'strand_stats.skew)

(deftest skew-test
  (is (= '(0 0 -1 0 0 -1) (strand_stats.skew/skew "ACGAC")))
  (is (= '(0 -1 -1 -1 0 1 2 1 1 1 0 1 2 1 0 0 0 0 -1 0 -1 -2)
         (strand_stats.skew/skew "CATGGGCATCGGCCATACGCC")))
  )
