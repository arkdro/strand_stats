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

(deftest add-one-pattern-pos-test
  (is (= [0]
         (strand_stats.approx_match/add-one-pattern-pos
          0 3 [] #{"ATC" "AGC" "ACC" "AAC"} "AGCTATC")))
  (is (= [0 4]
         (strand_stats.approx_match/add-one-pattern-pos
          4 3 [0] #{"ATC" "AGC" "ACC" "AAC"} "AGCTATC")))
  )

(deftest find-all-k-mers-test
  (is (= ["AGC" "ATC" "CTA" "GCT" "TAT"]
         (sort
          (strand_stats.approx_match/find-all-k-mers 3 "AGCTATC"))))
  )

(deftest build-all-approx-patterns-test
  (let [exp {"CGA" #{"AGA" "CAA" "CCA" "CGA" "CGC"
                     "CGG" "CGT" "CTA" "GGA" "TGA"}
             "TAT" #{"AAT" "CAT" "GAT" "TAA" "TAC"
                     "TAG" "TAT" "TCT" "TGT" "TTT"}}]
    (is (= exp
           (strand_stats.approx_match/build-all-approx-patterns
            1 ["CGA" "TAT"])))
    )
  )

(deftest find-freq-approx-match-test
  (is (= {"CGA" 1}
         (strand_stats.approx_match/find-freq-approx-match
          3 1 "CGA")))
  (is (= {"CGA" 2, "GAT" 1, "ATG" 1, "TGA" 2}
         (strand_stats.approx_match/find-freq-approx-match
          3 1 "CGATGA")))
  )

(deftest most-freq-approx-match-test
  (is (= ["CGA" "TGA"]
         (sort
          (strand_stats.approx_match/most-freq-approx-match
           3 1 "CGATGA"))))
  (is (= ["ATGC" "ATGT" "GATG"]
         (sort
          (strand_stats.approx_match/most-freq-approx-match
           4 1 "ACGTTGCATGTCGCATGATGCATGAGAGCT"))))
  )

(deftest increase-every-neighbour-test
  (is (= {}
          (strand_stats.approx_match/increase-every-neighbour
           "ACG"
           {"ACA" #{"ACA" "ACC" "ACG" "ACT"}}
           {})))
  (is (= {"ACA" 1, "ACC" 1, "ACG" 1, "ACT" 1}
          (strand_stats.approx_match/increase-every-neighbour
           "ACA"
           {"ACA" #{"ACA" "ACC" "ACG" "ACT"}}
           {})))
  (is (= {"ACA" 2, "ACC" 1, "ACG" 1, "ACT" 1, "CCA" 1, "GCA" 1, "TCA" 1}
          (strand_stats.approx_match/increase-every-neighbour
           "TCA"
           {"TCA" #{"ACA" "CCA" "GCA" "TCA"}}
           {"ACA" 1, "ACC" 1, "ACG" 1, "ACT" 1})))
  )

(deftest find-freq-approx-match2-test
  (is (= {"GGA" 1, "CCA" 1, "CGG" 1, "CAA" 1, "TGA" 1,
          "CGT" 1, "CTA" 1, "AGA" 1, "CGA" 1, "CGC" 1}
         (strand_stats.approx_match/find-freq-approx-match2
          3 1 "CGA")))
  (is (= {"TGT" 1, "AGG" 1, "TTA" 1, "ATT" 1, "GGA" 2, "CCA" 1, "CGG" 1,
          "CAA" 1, "ACG" 1, "AAG" 1, "GTT" 1, "TTG" 1, "GAA" 1, "TAT" 1,
          "GAC" 1, "GAG" 1, "TGA" 2, "ATA" 1, "TGC" 1, "CGT" 1, "CTA" 1,
          "ATC" 1, "TCA" 1, "TGG" 1, "AAT" 1, "ATG" 1, "GGT" 1, "TAA" 1,
          "CTG" 1, "CAT" 1, "GCT" 1, "GTG" 1, "GAT" 1, "AGA" 2, "CGA" 2,
          "CGC" 1}
         (strand_stats.approx_match/find-freq-approx-match2
          3 1 "CGATGA")))
  (is (= {"AAA" 1, "TGT" 1, "AGG" 2, "TTA" 1, "ATT" 1, "GGA" 3,
          "CCA" 1, "CGG" 1, "CAA" 1, "ACG" 1, "AAG" 2, "GTT" 1,
          "TTG" 1, "GAA" 2, "GGG" 1, "TAT" 1, "GAC" 2, "CAG" 1,
          "GCG" 1, "GAG" 2, "TGA" 3, "AGT" 1, "ATA" 2, "TGC" 1,
          "CGT" 1, "CTA" 1, "ATC" 1, "TCA" 1, "TGG" 1, "AAT" 1,
          "ATG" 1, "GGT" 1, "TAA" 1, "CTG" 1, "CAT" 1, "GCT" 1,
          "GTG" 2, "GAT" 2, "TAG" 1, "AGA" 3, "CGA" 3, "AGC" 1,
          "CGC" 1, "ACA" 1}
         (strand_stats.approx_match/find-freq-approx-match2
          3 1 "CGAGATGA")))
  )

(deftest most-freq-approx-match2-test
  (is (= ["ATGC" "ATGT" "GATG"]
         (sort
          (strand_stats.approx_match/most-freq-approx-match2
           4 1 "ACGTTGCATGTCGCATGATGCATGAGAGCT"))))
  (is (= ["AAAAA"]
         (sort
          (strand_stats.approx_match/most-freq-approx-match2
           5 1 "AACAAGCTGATAAACATTTAAAGAG"))))
  )

