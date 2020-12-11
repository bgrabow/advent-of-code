(ns advent-of-code.solution.day-07
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-valid-parent
  [rule]
  (let [[[_ parent children]] (re-seq #"(.+) bags contain (.+)" rule)]
    (into {} (for [[[_ _n color]] (->> (str/split children #", ")
                                       (map #(re-seq #"(\d+) (.+) bag" %)))]
               [color #{parent}]))))

(def valid-parents
  (->> (slurp "resources/input/day-07.txt")
       (str/split-lines)
       (map parse-valid-parent)
       (apply merge-with set/union)))

(def part-1
  (->> (iterate (fn [acc]
                  (let [new-frontier (set/difference (get valid-parents
                                                          (first (:frontier acc)))
                                                     (:visited acc))]
                    {:frontier (into (subvec (:frontier acc) 1) new-frontier)
                     :visited (into (:visited acc) new-frontier)}))
                {:frontier ["shiny gold"]
                 :visited #{}})
       (drop-while #(seq (:frontier %)))
       first
       :visited
       count))

(defn parse-required-children
  [rule]
  (let [[[_ parent children]] (re-seq #"(.+) bags contain (.+)" rule)]
    [parent (into {} (for [[[_ n color]] (->> (str/split children #", ")
                                              (map #(re-seq #"(\d+) (.+) bag" %)))
                           :when (some? color)]
                       [color (Long/parseLong n)]))]))

(def required-children
  (->> (slurp "resources/input/day-07.txt")
       (str/split-lines)
       (map parse-required-children)
       (into {})))

(def part-2
  (->> (iterate (fn [acc]
                  (let [[color count] (first (:frontier acc))
                        required (->> (get required-children color)
                                      (map (fn [[k v]] [k (* v count)]))
                                      (into {}))]
                    {:frontier (into (subvec (:frontier acc) 1) required)
                     :required (merge-with + (:required acc) required)}))

                {:frontier [["shiny gold" 1]]
                 :required {}})
       (drop-while #(seq (:frontier %)))
       first
       :required
       vals
       (reduce +)))

