(ns advent-of-code.solution.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(let [foods (->> (slurp "resources/input/day-21.txt")
                 (str/split-lines)
                 (map #(->> (str/split % #"\(contains")
                            (map (partial re-seq #"\w+")))))]
  (->> (for [[ingredients allergens] foods]
         (into {} (map #(vector % (set ingredients)) allergens)))
       (apply merge-with set/intersection)))

(def part-1
  (let [allergen-translations {"wheat" #{"kqms"},
                               "eggs" #{"bjq"},
                               "nuts" #{"klplr"},
                               "shellfish" #{"tlgjzx"},
                               "peanuts" #{"dtvhzt"},
                               "sesame" #{"sbzd"},
                               "soy" #{"ctmbr"},
                               "fish" #{"jznhvh"}}
        foods (->> (slurp "resources/input/day-21.txt")
                   (str/split-lines)
                   (map #(->> (str/split % #"\(contains")
                              (map (partial re-seq #"\w+")))))
        allergic-ingredients (set (for [[_ ingredients] allergen-translations]
                                    (first (vec ingredients))))]
    (reduce + (for [[ingredients] foods]
                (count (set/difference (set ingredients) allergic-ingredients))))))


(def part-2
  (let [sorted-allergens (sort-by first {"wheat" #{"kqms"},
                                         "eggs" #{"bjq"},
                                         "nuts" #{"klplr"},
                                         "shellfish" #{"tlgjzx"},
                                         "peanuts" #{"dtvhzt"},
                                         "sesame" #{"sbzd"},
                                         "soy" #{"ctmbr"},
                                         "fish" #{"jznhvh"}})]
    (->> (map second sorted-allergens)
         (map #(first (vec %)))
         (str/join ","))))
