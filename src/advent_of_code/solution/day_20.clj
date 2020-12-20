(ns advent-of-code.solution.day-20
  (:require [clojure.string :as str]))

(defn parse-tile
  [s]
  (let [[title & rows] (str/split-lines s)]
    [(Long/parseLong (re-find #"\d+" title))
     (into {} (mapcat (fn [y row]
                        (map-indexed
                          (fn [x cell]
                            [[x y] cell])
                          row))
                      (range)
                      rows))]))

(def edges
  [(mapv vector (repeat 0) (range 10))
   (mapv vector (repeat 9) (range 10))
   (mapv vector (range 10) (repeat 0))
   (mapv vector (range 10) (repeat 9))])

(def part-1
  (let [tiles (->> (map parse-tile (str/split (slurp "resources/input/day-20.txt") #"\n\n")))
        tile-edges (->> tiles
                        (map (fn [[id tile]]
                               [id (mapcat #(vector
                                              (map tile (reverse %))
                                              (map tile %)) edges)])))
        outer-edges (->> tiles
                         (map (fn [[id tile]]
                                (mapcat #(vector
                                           (map tile (reverse %))
                                           (map tile %)) edges)))
                         (apply concat)
                         frequencies
                         (keep (fn [[edge f]]
                                 (when (= 1 f)
                                   edge)))
                         set)]
    (->> (keep (fn [[id edges]]
                 (when (= 4 (count (filter outer-edges edges)))
                   id))
               tile-edges)
         (reduce *))))
