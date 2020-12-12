(ns advent-of-code.solution.day-10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combinatorics]))

(def adapters
  (#(conj % (+ 3 (apply max %)))
    (vec (sort (conj (->> (slurp "resources/input/day-10.txt")
                          (str/split-lines)
                          (map #(Long/parseLong %)))
                     0)))))

(def part-1
  (->> adapters
       ((juxt next identity))
       (apply map -)
       frequencies
       ((juxt #(get % 1) #(get % 3)))
       (apply *)))

(defn valid-adapter?
  [adapter-chain]
  (->> adapter-chain
       ((juxt next identity))
       (apply map -)
       (every? #(<= % 3))))

(defn combinations
  "The number of combinations of adapters that are valid, given a set of `n` adapters with output joltages
  contiguously separated by 1 jolt."
  [n]
  (->> (for [size (range n)]
         (combinatorics/combinations (drop-last (drop 1 (range n))) size))
       (apply concat)
       (map #(sort (conj % 0 (dec n))))
       (filter valid-adapter?)
       count))

(def part-2
  (->> adapters
       ((juxt next identity))
       (apply map -)
       (partition-by #(= % 1))
       (map frequencies)
       (map #(update % 1 (fnil inc 0)))
       (map #(get % 1))
       (map combinations)
       (reduce *)))
