(ns advent-of-code.solution.day-18
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn calculate
  [l]
  (if (list? l)
    (let [[lhs op rhs & more] l]
      (if op
        (calculate (apply list ((resolve op) (calculate lhs) (calculate rhs)) more))
        lhs))
    l))

(def part-1
  (->> (str/split-lines (slurp "resources/input/day-18.txt"))
       (map #(calculate (edn/read-string (str "(" % ")"))))
       (reduce +)))
