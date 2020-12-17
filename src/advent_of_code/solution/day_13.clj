(ns advent-of-code.solution.day-13
  (:require [clojure.string :as str]))

(def part-1
  (let [[departure-time buses] (str/split-lines (slurp "resources/input/day-13.txt"))
        departure-time (Long/parseLong departure-time)
        buses (-> buses
                  (str/split #",")
                  (->> (remove #{"x"})
                       (map #(Long/parseLong %))))]
    (->> (for [bus buses]
           [bus (- bus (rem departure-time bus))])
         (apply min-key second)
         (apply *))))

(def part-2)

(def bus-time-pairs
  (->> (map vector
            (str/split
              (second
                (str/split-lines
                  (slurp "resources/input/day-13.txt")))
              #",")
            (range))
       (remove (comp #{"x"} first))
       (map (fn [[bus t]] [(Long/parseLong bus) t]))))

(def time-offset 17)

(def offset-bus-time-pairs
  (for [[bus time] bus-time-pairs]
    [bus (- time time-offset)]))

(def reduced-bus-time-pairs
  (sort-by second (for [[bus time] offset-bus-time-pairs]
                    [bus (rem time bus)])))

(def part-2
  (let [[simultaneous-buses separate-buses] ((juxt #(get % true) #(get % false))
                                             (group-by (fn [[_bus time]] (zero? time)) reduced-bus-time-pairs))
        major-interval (reduce * (map first simultaneous-buses))
        _minor-interval (reduce * (map first separate-buses))
        lower-bound 100000000000000
        first-guess (* major-interval (inc (quot lower-bound major-interval)))
        repetition-interval (reduce * (map first bus-time-pairs))
        search-space (range first-guess repetition-interval major-interval)
        separate-buses (for [[bus delay] (sort-by (comp - first) separate-buses)]
                         [bus (if (neg? delay)
                                (+ delay bus)
                                delay)])
        offset-answer (->> search-space
                           (filter #(every? (fn [[bus delay]]
                                              (= delay (- bus (rem % bus))))
                                            separate-buses))
                           first)]
    (- offset-answer time-offset)))
