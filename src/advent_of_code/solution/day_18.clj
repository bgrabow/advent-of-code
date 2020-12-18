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
       (map #(edn/read-string (str "(" % ")")))
       (map calculate)
       (reduce +)))

(defn calculate-2
  [l]
  (if (list? l)
    (let [[lhs op rhs & more] l]
      (if op
        (cond
          (#{'+} op) (calculate-2
                       (apply list
                              ((resolve op) (calculate-2 lhs) (calculate-2 rhs))
                              more))
          (empty? more) ((resolve op) (calculate-2 lhs) (calculate-2 rhs))
          :else (calculate-2 (list lhs op (apply list rhs more))))
        lhs))
    l))

(def part-2
  (->> (str/split-lines (slurp "resources/input/day-18.txt"))
       (map #(edn/read-string (str "(" % ")")))
       (map calculate-2)
       (reduce +)))
