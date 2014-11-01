(ns strand_stats.skew
  ;; (:require bia_utils.util)
  )

(defn skew-aux [lst cnt acc-lst]
  (if (empty? lst) (reverse acc-lst)
      (case (first lst)
        \C (recur (rest lst) (dec cnt) (cons (dec cnt) acc-lst))
        \G (recur (rest lst) (inc cnt) (cons (inc cnt) acc-lst))
        \A (recur (rest lst) cnt (cons cnt acc-lst))
        \T (recur (rest lst) cnt (cons cnt acc-lst))
        )
      )
  )

(defn skew [text]
  (skew-aux (seq text) 0 '(0)))

