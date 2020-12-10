(ns advent-of-code.solution.day-05
  (:require [clojure.string :as str]))

(defn parse-row
  [row]
  (Long/parseLong (apply str (map {\B "1" \F "0"} row)) 2))

(defn parse-column
  [row]
  (Long/parseLong (apply str (map {\R "1" \L "0"} row)) 2))

(defn seat-id
  [[row column]]
  (+ column (* 8 row)))

(defn parse-boarding-pass
  [[row column]]
  [(parse-row row) (parse-column column)])

(def part-1
  (->> (slurp "resources/input/day-05.txt")
       (str/split-lines)
       (map #(split-at 7 %))
       (map parse-boarding-pass)
       (map seat-id)
       (apply max)))

(def part-2
  (let [seat-ids (->> (slurp "resources/input/day-05.txt")
                      (str/split-lines)
                      (map #(split-at 7 %))
                      (map parse-boarding-pass)
                      (map seat-id)
                      sort)]
    (first (keep (fn [[seat-ahead gap]]
                   (when (> gap 1)
                     (inc seat-ahead)))
                 (zipmap seat-ids (map - (next seat-ids) seat-ids))))))
