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
  (let [tiles (map parse-tile (str/split (slurp "resources/input/day-20.txt") #"\n\n"))
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
                         set)
        corner-ids (->> (keep (fn [[id edges]]
                                (when (= 4 (count (filter outer-edges edges)))
                                  id))
                              tile-edges))]
    (reduce * corner-ids)))

(defn rotate-right
  [m]
  (into {}
        (for [[[x y] c] m]
          [[(- 9 y) x] c])))

(defn rotations
  [contents]
  (take 4 (iterate rotate-right contents)))

(defn flip
  [contents]
  (into {} (for [[[x y] c] (vec contents)]
             [[(- 9 x) y] c])))

#_(rotate-right (second (first (map parse-tile (str/split (slurp "resources/input/day-20.txt") #"\n\n")))))

(let [tiles (map parse-tile (str/split (slurp "resources/input/day-20.txt") #"\n\n"))
      edge-index (->> (mapcat (fn [[_id contents]]
                                (for [contents (concat (rotations contents)
                                                       (rotations (flip contents)))]
                                  {:left {(mapv contents (edges 0)) contents}
                                   :right {(mapv contents (edges 1)) contents}
                                   :top {(mapv contents (edges 2)) contents}
                                   :bottom {(mapv contents (edges 2)) contents}}))
                              tiles)
                      (apply merge-with merge))])

;; Choose first corner.
;; Find rotation for top left.
;; Iterate down column finding next tiles.
;; For each row, iterate across row finding next tiles.