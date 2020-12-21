(ns advent-of-code.solution.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn winnow-possibilities
  [m]
  (->> (iterate #(let [decided-possibility (first
                                             (keep
                                               (fn [[_a possibilities]]
                                                 (when (= 1 (count possibilities))
                                                   (first (seq possibilities))))
                                               %))]
                   (map (fn [[a possibilities]]
                          [a (disj (set possibilities) decided-possibility)])
                        %))
                (seq m))
       (map (fn [m]
              (first
                (keep
                  (fn [[a possibilities]]
                    (when (= 1 (count possibilities))
                      [a (first (seq possibilities))]))
                  m))))
       (take-while some?)
       (into {})))

(def allergen-translations
  (let [foods (->> (slurp "resources/input/day-21.txt")
                   (str/split-lines)
                   (map #(->> (str/split % #"\(contains")
                              (map (partial re-seq #"\w+")))))
        possible-allergens (->> (for [[ingredients allergens] foods]
                                  (into {} (map #(vector % (set ingredients)) allergens)))
                                (apply merge-with set/intersection))]
    (winnow-possibilities possible-allergens)))

(def part-1
  (let [foods (->> (slurp "resources/input/day-21.txt")
                   (str/split-lines)
                   (map #(->> (str/split % #"\(contains")
                              (map (partial re-seq #"\w+")))))
        allergic-ingredients (set (map second allergen-translations))]
    (reduce + (for [[ingredients] foods]
                (count (set/difference (set ingredients) allergic-ingredients))))))


(def part-2
  (let [sorted-allergens (sort-by first allergen-translations)]
    (->> (map second sorted-allergens)
         (str/join ","))))
