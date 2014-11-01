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

(defn find-skew-min-aux [idx cur-min acc [head & tail]]
  (cond
    (nil? head) acc
    (< head cur-min) (recur (inc idx) head [idx] tail)
    (= head cur-min) (recur (inc idx) cur-min (conj acc idx) tail)
    :default (recur (inc idx) cur-min acc tail)))

(defn find-skew-min [text]
  (let [skew-data (skew text)]
    (find-skew-min-aux 0 0 [] skew-data)))

