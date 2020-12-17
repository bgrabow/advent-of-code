(ns advent-of-code.solution.day-17
  (:require [clojure.string :as str]))

(defn parse-initial-live-cells
  []
  (->> (slurp "resources/input/day-17.txt")
       (str/split-lines)
       (mapcat (fn [y row]
                 (map (fn [x cell]
                        [[x y 0 0] cell])
                      (range)
                      row))
               (range))
       (keep (fn [[p cell]]
               (when (#{\#} cell)
                 p)))
       (into #{})))

(defn neighbors-3d
  [[x y z]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not= [0 0 0] [dx dy dz])]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn neighbors-4d
  [[x y z w]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]
        :when (not= [0 0 0 0] [dx dy dz dw])]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(defn step
  [neighbors live-cells]
  (let [life-force (->> live-cells
                        (mapcat neighbors)
                        (frequencies))]
    (set (keep (fn [[p n]]
                 (cond
                   (and (live-cells p) (#{2 3} n)) p
                   (and (not (live-cells p)) (#{3} n)) p))
               life-force))))

(def part-1
  (let [init (parse-initial-live-cells)]
    (count (first (drop 6 (iterate (partial step neighbors-3d) init))))))

(def part-2
  (let [init (parse-initial-live-cells)]
    (count (first (drop 6 (iterate (partial step neighbors-4d) init))))))
