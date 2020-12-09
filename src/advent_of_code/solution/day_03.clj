(ns advent-of-code.solution.day-03
  (:require [clojure.string :as str]))

(comment
  "Failed attempt at being clever"
  (defn parse-input
    []
    (->> (slurp "resources/input/day-03.txt")
         (str/split-lines)
         (map-indexed
           (fn [y row]
             (map-indexed
               (fn [x contents]
                 [[x y] contents])
               row)))
         (mapcat identity)
         (into {})))

  (defn path
    [initial [[xmin xmax] [ymin ymax]] step]
    (->> (iterate #(mapv + % step) initial)
         (take-while (fn [[x y]]
                       (and x y
                            (<= xmin x xmax)
                            (<= ymin y ymax))))))

  (defn x-tiled-path
    [initial [[xmin xmax] [ymin ymax]] step]
    (->> (iterate #(mapv + % step) initial)
         (take-while (fn [[x y]]
                       (and x y
                            (<= ymin y ymax))))
         (map (fn [[x y]]
                [(rem x xmax) y]))))

  (def part-1
    (let [forest (parse-input)
          [xmin ymin] [0 0]
          xmax (apply max (map first (keys forest)))
          ymax (Long/MAX_VALUE)]
      (map forest (x-tiled-path
                    [0 0]
                    [[xmin xmax] [ymin ymax]]
                    [3 1])))))

(defn trees-in-slope
  [[dx dy]]
  (let [width (count (first (str/split-lines (slurp "resources/input/day-03.txt"))))]
    (-> (map (fn [line x]
               (.charAt ^String line x))
             (map first (partition 1 dy (str/split-lines (slurp "resources/input/day-03.txt"))))
             (map #(rem % width) (iterate #(+ dx %) 0)))
        frequencies
        (get \#))))

(def part-1
  (trees-in-slope [3 1]))

(def part-2
  (reduce * (map trees-in-slope [[1 1] [3 1] [5 1] [7 1] [1 2]])))
