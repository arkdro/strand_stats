(ns strand_stats.test.approx_match
  (:use [strand_stats.approx_match])
  (:use clojure.tools.trace)
  (:use [clojure.test]))

;; (trace-ns 'strand_stats.approx_match)

(deftest replace-one-char-test
  (is (= (vec "AGGAC")
         (strand_stats.approx_match/replace-one-char (vec "CGGAC") 0 \A)))
  (is (= (vec "CGGAA")
         (strand_stats.approx_match/replace-one-char (vec "CGGAC") 4 \A)))
  )

(deftest fill-one-position-test
  (is (= [(vec "CAGAC")
          (vec "CCGAC")
          (vec "CGGAC")
          (vec "CTGAC")]
         (strand_stats.approx_match/fill-one-position 1 (vec "CGGAC"))))
  (is (= [(vec "CGGAA")
          (vec "CGGAC")
          (vec "CGGAG")
          (vec "CGGAT")]
         (strand_stats.approx_match/fill-one-position 4 (vec "CGGAC"))))
  )

(deftest build-one-approx-pattern-test
  (is (= ["CAGAC" "CCGAC" "CGGAC" "CTGAC"]
         (strand_stats.approx_match/build-one-approx-pattern
          [1] (vec "CGGAC"))))
  (is (= [
          ;; 0 1
          "AAA"
          "ACA"
          "AGA"
          "ATA"
          ;; 0 1
          "CAA"
          "CCA"
          "CGA"
          "CTA"
          ;; 0 1
          "GAA"
          "GCA"
          "GGA"
          "GTA"
          ;; 0 1
          "TAA"
          "TCA"
          "TGA"
          "TTA"
          ]
         (strand_stats.approx_match/build-one-approx-pattern
          [0 1] (vec "CGA"))))
  )

(deftest calc-combinations-test
  (is (= [[0] [1] [2] [3] [0 1] [0 2] [0 3] [1 2] [1 3] [2 3]]
         (strand_stats.approx_match/calc-combinations 2 "CAGA")))
  (is (= [[0] [1] [2] [0 1] [0 2] [1 2]]
         (strand_stats.approx_match/calc-combinations 2 "CGA")))
  )

