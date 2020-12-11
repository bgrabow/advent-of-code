(ns advent-of-code.solution.day-09
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def signal
  (mapv #(Long/parseLong %)
    (str/split-lines
      (slurp "resources/input/day-09.txt"))))

(def window-size 25)

;; No repeated numbers in any window
;; (= [25] (keys (frequencies (map #(count (set %)) (partition 25 1 signal)))))

(def windows
  (reductions (fn [window [outgoing incoming]]
                (-> window
                    (disj outgoing)
                    (conj incoming)))
              (set (take window-size signal))
              (map vector signal (drop window-size signal))))

(defn valid-xmas?
  [window x]
  (let [complements (set (map #(- x %) window))]
    (not (empty? (set/intersection complements window)))))

(def part-1
  (let [[_window value] (first
                          (remove #(apply valid-xmas? %)
                                  (map vector windows (drop window-size signal))))]
    value))

(def part-2
  (let [{:keys [low high]} (->> (iterate (fn [{:keys [sum low high] :as acc}]
                                           (cond
                                             (= part-1 sum) acc
                                             (< part-1 sum) {:sum (- sum (get signal low))
                                                             :low (inc low)
                                                             :high high}
                                             (> part-1 sum) {:sum (+ sum (get signal high))
                                                             :low low
                                                             :high (inc high)}))
                                         {:sum 0
                                          :low 0
                                          :high 0})
                                (drop-while #(not= part-1 (:sum %)))
                                first)
        window (subvec signal low high)]
    (+ (apply min window) (apply max window))))
