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

(deftest build-approx-patterns-test
  (let [
        exp-list [
                  ;; 0
                  "AGA"
                  "CGA"
                  "GGA"
                  "TGA"
                  ;; 1
                  "CAA"
                  "CCA"
                  "CGA"
                  "CTA"
                  ;; 2
                  "CGA"
                  "CGC"
                  "CGG"
                  "CGT"

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
                  ;; 0 2
                  "AGA"
                  "AGC"
                  "AGG"
                  "AGT"
                  ;; 0 2
                  "CGA"
                  "CGC"
                  "CGG"
                  "CGT"
                  ;; 0 2
                  "GGA"
                  "GGC"
                  "GGG"
                  "GGT"
                  ;; 0 2
                  "TGA"
                  "TGC"
                  "TGG"
                  "TGT"

                  ;; 1 2
                  "CAA"
                  "CAC"
                  "CAG"
                  "CAT"
                  ;; 1 2
                  "CCA"
                  "CCC"
                  "CCG"
                  "CCT"
                  ;; 1 2
                  "CGA"
                  "CGC"
                  "CGG"
                  "CGT"
                  ;; 1 2
                  "CTA"
                  "CTC"
                  "CTG"
                  "CTT"
                  ]
        exp (into #{} exp-list)
        ]
    (is (= exp
           (strand_stats.approx_match/build-approx-patterns 2 "CGA")))
    )
  )

(deftest approx-matched-test
  (is (= false (strand_stats.approx_match/approx-matched "ASDF" #{"ZXCV"})))
  (is (= true (strand_stats.approx_match/approx-matched "ASDF" #{"ASDF"})))
  )

(deftest add-one-pattern-test
  (is (= {"ASD" 1}
         (strand_stats.approx_match/add-one-pattern 0 3 {} "ASD" #{} "ASD")))
  (is (= {"ABS" 1}
         (strand_stats.approx_match/add-one-pattern
          0 3 {} "ABS"
          #{"ASF" "ASD"} "ASDF")))
  (is (= {"ABS" 3}
         (strand_stats.approx_match/add-one-pattern
          0 3 {"ABS" 2} "ABS"
          #{"ASF" "ASD"} "ASDF")))
  (is (= {"ABS" 2, "AAF" 1}
         (strand_stats.approx_match/add-one-pattern
          0 3 {"ABS" 2} "ABS"
          #{"ASF" "ASD"} "AAFF")))
  )

(deftest iter-over-text-test
  (is (= [0 2]
         (strand_stats.approx_match/iter-over-text
          "ABS" #{"ABS" "ABD" "DFA"} "ABDFA")))
  )

(deftest find-approx-match-test
  (is (= [0 4]
         (strand_stats.approx_match/find-approx-match
          1 "ATC" "AGCTATC")))
  )

