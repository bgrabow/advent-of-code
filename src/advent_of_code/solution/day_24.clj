(ns advent-of-code.solution.day-24
  (:require [clojure.string :as str]))

(def direction-vector
  {"e" [2 0]
   "w" [-2 0]
   "se" [1 1]
   "sw" [-1 1]
   "ne" [1 -1]
   "nw" [-1 -1]})

(defn parse-directions
  [s]
  (->> (re-seq #"(?:e|w|se|sw|ne|nw)" s)
       (map direction-vector)
       (reduce (partial mapv +))))

(def part-1
  (->> (str/split-lines (slurp "resources/input/day-24.txt"))
       (map parse-directions)
       (frequencies)
       (filter (fn [[_p n]]
                 (odd? n)))
       (count)))

(defn neighbors
  [p]
  (map #(mapv + p %) (vals direction-vector)))

(defn step
  [black-tiles]
  (->> (mapcat neighbors black-tiles)
       (frequencies)
       (filter (fn [[p n]]
                 (or (and (#{1 2} n)
                          (black-tiles p))
                     (and (#{2} n)
                          (not (black-tiles p))))))
       (map first)
       (into #{})))

(def part-2
  (let [init (->> (str/split-lines (slurp "resources/input/day-24.txt"))
                  (map parse-directions)
                  (frequencies)
                  (filter (fn [[_p n]]
                            (odd? n)))
                  (map first)
                  (into #{}))]
    (->> (iterate step init)
         (drop 100)
         (first)
         (count))))
