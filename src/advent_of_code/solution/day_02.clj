(ns advent-of-code.solution.day-02
  (:require [clojure.string :as str]))

(defn parse-input
  []
  (->> (slurp "resources/input/day-02.txt")
       (str/split-lines)
       (map (fn [s]
              (let [[[_ low high c password]] (re-seq #"(\d+)\-(\d+)\s(\w):\s(\w+)" s)]
                [(Long/parseLong low)
                 (Long/parseLong high)
                 (first c)
                 password])))))

(defn valid-password-policy-one?
  [[low high c password]]
  (let [fs (frequencies password)]
    (<= low (or (fs c) 0) high)))

(defn valid-password-policy-two?
  [[x y c password]]
  (= 1 (count
         (filter
           #(= c (nth password (dec %)))
           [x y]))))

(def part-1
  (count (filter valid-password-policy-one? (parse-input))))

(def part-2
  (count (filter valid-password-policy-two? (parse-input))))
