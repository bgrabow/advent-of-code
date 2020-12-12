(ns advent-of-code.solution.day-11
  (:require [clojure.string :as str]))

(def example-input
  "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

(def seats
  (->> (slurp "resources/input/day-11.txt")
       (str/split-lines)
       (map-indexed
         (fn [y line]
           (map-indexed
             (fn [x cell]
               [[x y] cell])
             line)))
       (apply concat)
       (filter #(= \L (second %)))
       (into {})))

(def example-seats
  (->> example-input
       (str/split-lines)
       (map-indexed
         (fn [y line]
           (map-indexed
             (fn [x cell]
               [[x y] cell])
             line)))
       (apply concat)
       (filter #(= \L (second %)))
       (into {})))

(defn neighbors
  [[x y]]
  (for [x' [(dec x) x (inc x)]
        y' [(dec y) y (inc y)]
        :when (not= [x y] [x' y'])]
    [x' y']))

(defn step
  [seats]
  (->> (keys seats)
       (reduce (fn [acc seat]
                 (let [nearby-seats (frequencies (map seats (neighbors seat)))]
                   (assoc acc seat (cond
                                     (and (= \L (get seats seat))
                                          (zero? (get nearby-seats \# 0))) \#
                                     (and (= \# (get seats seat))
                                          (>= (get nearby-seats \# 0) 4)) \L
                                     :else (get seats seat)))))
               seats)))

(defn iterate-until-fixed
  [f init]
  (reduce (fn [prev next]
            (if (= prev next)
              (reduced next)
              next))
          (iterate f init)))

(->> (map (juxt identity step) (iterate step seats))
     second)

(def final-seats (iterate-until-fixed step seats))

(def part-1
  (get (->> final-seats
            vals
            frequencies)
       \#))
