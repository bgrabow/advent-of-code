(ns advent-of-code.solution.day-12
  (:require [clojure.string :as str]))

(def instructions
  (map #(let [[[_ op val]] (re-seq #"([A-Z]+)(\d+)" %)]
          [op (Long/parseLong val)])
       (str/split-lines (slurp "resources/input/day-12.txt"))))

(defn turn-left
  [heading turn-amount]
  (nth (get {"N" ["W" "S" "E" "N" "W" "S" "E" "N"]
             "W" ["S" "E" "N" "W" "S" "E" "N"]
             "S" ["E" "N" "W" "S" "E" "N"]
             "E" ["N" "W" "S" "E" "N"]}
            heading)
       (dec (quot turn-amount 90))))

(defn turn-right
  [heading turn-amount]
  (nth (get {"N" ["E" "S" "W" "N"]
             "W" ["N" "E" "S" "W"]
             "S" ["W" "N" "E" "S"]
             "E" ["S" "W" "N" "E"]}
            heading)
       (dec (quot turn-amount 90))))

(defn step
  [[heading location] [op val]]
  (case op
    "N" [heading (mapv - location [0 val])]
    "S" [heading (mapv + location [0 val])]
    "E" [heading (mapv + location [val 0])]
    "W" [heading (mapv - location [val 0])]
    "L" [(turn-left heading val) location]
    "R" [(turn-right heading val) location]
    "F" [heading (mapv + location (case heading
                                    "N" [0 (- val)]
                                    "S" [0 val]
                                    "E" [val 0]
                                    "W" [(- val) 0]))]))

(def part-1
  (let [[_ [x y]] (reduce step ["E" [0 0]] instructions)]
    (+ x y)))
