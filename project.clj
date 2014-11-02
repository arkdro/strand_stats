(defproject strand_stats "1.0.0-SNAPSHOT"
  :description "half-strands stats. Bioinformatics algorithms"
  :main strand_stats.core
  :extra-classpath-dirs [
                         ".lein-git-deps/bia_utils/src"
                         ]
  :dependencies [
                  [org.clojure/clojure "1.4.0"]
                  [clj-getopts "0.0.2"]
                  [org.clojure/tools.trace "0.7.5"]
                  [org.clojure/math.combinatorics "0.0.8"]
                ])

