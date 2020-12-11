(ns advent-of-code.solution.day-08
  (:require [clojure.string :as str]))

(def instructions (str/split-lines (slurp "resources/input/day-08.txt")))

(def part-1
  (->> (iterate (fn [{:keys [acc pos]}]
                  (let [[[_ op arg]] (re-seq #"(\w+) (.+)" (get instructions pos))
                        arg (Long/parseLong arg)]
                    (case op
                      "nop" {:acc acc :pos (inc pos)}
                      "acc" {:acc (+ acc arg) :pos (inc pos)}
                      "jmp" {:acc acc :pos (+ pos arg)})))
                {:acc 0
                 :pos 0})
       (reduce (fn [visited {:keys [acc pos]}]
                 (if (visited pos)
                   (reduced acc)
                   (conj visited pos)))
               #{})))

(def part-2
  (first (keep identity
               (for [n (range (count instructions))]
                 (let [[acc pos] (->> (iterate (fn [{:keys [acc pos]}]
                                                 (if (get instructions pos)
                                                   (let [[[_ op arg]] (re-seq #"(\w+) (.+)" (get instructions pos))
                                                         arg (Long/parseLong arg)]
                                                     (if (= pos n)
                                                       (case op
                                                         "nop" {:acc acc :pos (+ pos arg)}
                                                         "acc" {:acc (+ acc arg) :pos (inc pos)}
                                                         "jmp" {:acc acc :pos (inc pos)})
                                                       (case op
                                                         "nop" {:acc acc :pos (inc pos)}
                                                         "acc" {:acc (+ acc arg) :pos (inc pos)}
                                                         "jmp" {:acc acc :pos (+ pos arg)})))
                                                   {:acc acc :pos pos}))
                                               {:acc 0
                                                :pos 0})
                                      (reduce (fn [visited {:keys [acc pos]}]
                                                (if (= pos (count instructions))
                                                  [acc pos])
                                                (if (visited pos)
                                                  (reduced [acc pos])
                                                  (conj visited pos)))
                                              #{}))]
                   (when (= pos (count instructions))
                     acc))))))
