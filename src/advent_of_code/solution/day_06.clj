(ns advent-of-code.solution.day-06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def part-1
  (->> (for [[group] (->> (slurp "resources/input/day-06.txt")
                          (str/split-lines)
                          (partition-by #(= "" %))
                          (partition-all 1 2))]
         (reduce into #{} group))
       (map count)
       (reduce +)))

(def part-1
  (->> (for [[group] (->> (slurp "resources/input/day-06.txt")
                          (str/split-lines)
                          (partition-by #(= "" %))
                          (partition-all 1 2))]
         (apply set/intersection (map set group)))
       (map count)
       (reduce +)))
