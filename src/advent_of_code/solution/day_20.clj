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

(defn find-outer-edges
  [tiles]
  (->> tiles
       (map (fn [[id tile]]
              (mapcat #(vector
                         (map tile (reverse %))
                         (map tile %)) edges)))
       (apply concat)
       frequencies
       (keep (fn [[edge f]]
               (when (= 1 f)
                 edge)))
       set))

(defn all-edges
  [tiles]
  (->> tiles
       (map (fn [[id tile]]
              [id (mapcat #(vector
                             (map tile (reverse %))
                             (map tile %)) edges)]))))

(defn find-corner-ids
  [tiles]
  (let [outer-edges (find-outer-edges tiles)]
    (->> tiles
         (all-edges)
         (keep (fn [[id edges]]
                 (when (= 4 (count (filter outer-edges edges)))
                   id))))))

(def part-1
  (let [tiles      (map parse-tile (str/split (slurp "resources/input/day-20.txt") #"\n\n"))
        corner-ids (find-corner-ids tiles)]
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

(defn edges-map
  [id tile]
  {:left   {(mapv tile (edges 0)) [[id tile]]}
   :right  {(mapv tile (edges 1)) [[id tile]]}
   :top    {(mapv tile (edges 2)) [[id tile]]}
   :bottom {(mapv tile (edges 3)) [[id tile]]}})

(defn tile-below
  [edge-index [origin-id origin-tile]]
  (->> (get-in edge-index [:top (mapv origin-tile (edges 3))])
       (remove (fn [[match-id]]
                 (= origin-id match-id)))
       (first)))

(defn tile-right
  [edge-index [origin-id origin-tile]]
  (->> (get-in edge-index [:left (mapv origin-tile (edges 1))])
       (remove (fn [[match-id]]
                 (= origin-id match-id)))
       (first)))

(defn print-tile
  [tile]
  (for [y (range 1 9)]
    (apply str (for [x (range 1 9)]
                 (tile [x y])))))

(defn trim-edges
  [tile]
  (reduce dissoc tile (for [x (range 10)
                            y (range 10)
                            :when (or (#{0 9} x)
                                      (#{0 9} y))]
                        [x y])))

(defn rotate-image
  [rows]
  (apply map (comp str/reverse str) rows))

(def sea-monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(defn count-monsters
  [rows]
  (->> (for [x (range (inc (- (count (first rows)) (count (first sea-monster)))))
             y (range (inc (- (count rows) (count sea-monster))))]
         (let [search-space (map #(drop x %) (drop y rows))]
           (mapcat (fn [monster-row image-row]
                     (map (fn [m s]
                            (or (= m \space)
                                (= m s)))
                          monster-row image-row))

                   sea-monster search-space)))
       (filter #(every? true? %))
       (count)))

(defn count-hashes
  [image]
  (reduce + (map #(count (filter #{\#} %)) image)))

(def part-2
  (let [tiles           (into {} (map parse-tile (str/split (slurp "resources/input/day-20.txt") #"\n\n")))
        edge-index      (->> (mapcat (fn [[id contents]]
                                       (for [contents (concat (rotations contents)
                                                              (rotations (flip contents)))]
                                         (edges-map id contents)))
                                     tiles)
                             (apply merge-with (partial merge-with into)))
        outer-edges     (find-outer-edges tiles)
        corner-ids      (find-corner-ids tiles)
        top-left-id     (first corner-ids)
        top-left-tile   (->> (tiles top-left-id)
                             (rotations)
                             (filter (fn [tile]
                                       (-> (edges-map top-left-id tile)
                                           ((juxt :top :left))
                                           (->> (mapcat keys)
                                                (remove outer-edges)
                                                (empty?)))))
                             (first))
        left-tiles      (->> (iterate (partial tile-below edge-index) [top-left-id top-left-tile])
                             (take-while some?))
        arranged-tiles  (mapv #(take-while some? (iterate (partial tile-right edge-index) %)) left-tiles)
        image           (->> arranged-tiles
                             (mapcat (fn [row]
                                       (->> row
                                            (map second)
                                            (map trim-edges)
                                            (map print-tile)
                                            (apply map str)))))
        image-rotations (take 4 (iterate rotate-image image))
        n-monsters      (apply max (map count-monsters image-rotations))
        sea-roughness   (- (count-hashes image) (* n-monsters (count-hashes sea-monster)))]
    sea-roughness))
