(ns advent-of-code.solution.day-01
  (:require [clojure.string :as str]))

(defn find-complements
  [xs sum]
  (let [expenses    xs
        complements (set (map #(- sum %) expenses))
        x           (first (filter #(contains? complements %) expenses))]
    (when x
      (* x (- sum x)))))

(def part-1
  (let [expenses (->> (slurp "resources/input/day-01.txt")
                      (str/split-lines)
                      (map #(Long/parseLong %)))]
    (find-complements expenses 2020)))

(def part-2
  (let [expenses (set (->> (slurp "resources/input/day-01.txt")
                           (str/split-lines)
                           (map #(Long/parseLong %))))
        target 2020
        threshold (/ target 3)
        x-candidates (filter #(< % threshold) expenses)]
    (->> (keep (fn [x]
                 (some-> (find-complements (disj expenses x) (- target x))
                         (* x)))
               x-candidates)
         (first))))
