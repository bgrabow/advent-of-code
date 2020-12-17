(ns advent-of-code.solution.day-16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def part-1
  (let [[validation-rules
         _my-ticket
         nearby-tickets] (->> (str/split-lines (slurp "resources/input/day-16.txt"))
                              (partition-by #{""})
                              (partition-all 1 2)
                              (apply concat))
        valid-ranges (->> (map #(map (partial drop 1) (re-seq #"(\d+)\-(\d+)" %)) validation-rules)
                          (flatten)
                          (map #(Long/parseLong %))
                          (partition 2))
        valid-numbers (->> (map #(apply range %) valid-ranges)
                           (reduce into #{}))
        nearby-tickets (->> nearby-tickets
                            (drop 1)
                            (map #(edn/read-string (str "[" % "]"))))]
    (->> nearby-tickets
         (mapcat #(remove valid-numbers %))
         (reduce +))))

(def part-2
  (let [[validation-rules
         my-ticket
         nearby-tickets] (->> (str/split-lines (slurp "resources/input/day-16.txt"))
                              (partition-by #{""})
                              (partition-all 1 2)
                              (apply concat))
        valid-ranges (->> (map #(map (partial drop 1) (re-seq #"(\d+)\-(\d+)" %)) validation-rules)
                          (flatten)
                          (map #(Long/parseLong %))
                          (partition 2))
        valid-numbers (->> (map #(apply range %) valid-ranges)
                           (reduce into #{}))
        nearby-tickets (->> nearby-tickets
                            (drop 1)
                            (map #(edn/read-string (str "[" % "]"))))
        valid-tickets (filter #(every? valid-numbers %) nearby-tickets)
        rules (->> validation-rules
                   (map #(let [[_ name] (re-find #"(.+)\:" %)
                               [[_ & r1] [_ & r2]] (re-seq #"(\d+)\-(\d+)" %)]
                           [name [r1 r2]]))
                   (map (fn [[name & ranges]]
                          [name (->> ranges
                                     (flatten)
                                     (map #(Long/parseLong %))
                                     (partition 2)
                                     (map #(range (first %) (inc (second %))))
                                     (reduce into #{}))]))
                   (into {}))
        candidate-fields (for [i (range (count (first valid-tickets)))]
                           (map first (filter (fn [[_name range]]
                                                (every? #(range (get % i)) valid-tickets))
                                              rules)))
        field-key (->> (iterate (fn [candidate-fields]
                                  (let [next-field (ffirst (filter #(= 1 (count %)) candidate-fields))]
                                    (map #(set/difference % #{next-field}) candidate-fields)))
                                (map set candidate-fields))
                       (take-while #(some not-empty %))
                       (mapcat #(keep-indexed (fn [i fields]
                                                (when (= 1 (count fields))
                                                  [(first fields) i])) %))
                       (into {}))
        departure-fields (into {} (filter (fn [[field-name]]
                                            (str/starts-with? field-name "departure")) field-key))
        my-ticket (edn/read-string (str "[" (second my-ticket) "]"))]
    (->> (map #(get my-ticket %) (vals departure-fields))
         (reduce *))))
