(ns advent-of-code.solution.day-15
  (:require [clojure.edn :as edn]))

(defn step
  [[xs seen i]]
  [(conj xs (if (seen (peek xs))
              (- i (first (seen (peek xs))))
              0))
   (update seen (peek xs) conj i)
   (inc i)])

(def part-1
  (let [init (edn/read-string (str "[" (slurp "resources/input/day-15.txt") "]"))
        seen (reduce (fn [seen [i n]]
                       (update seen n conj i))
                     {} (zipmap (next (range)) (butlast init)))]
    (let [[xs] (->> (iterate step [init seen (count init)])
                    (drop-while (fn [[_init _seen i]]
                                  (< i 2020)))
                    (first))]
      (last xs))))

(def part-2
  (let [init (edn/read-string (str "[" (slurp "resources/input/day-15.txt") "]"))
        seen (reduce (fn [seen [i n]]
                       (update seen n conj i))
                     {} (zipmap (next (range)) (butlast init)))]
    (let [[xs] (->> (iterate step [init seen (count init)])
                    (drop-while (fn [[_init _seen i]]
                                  (< i 30000000)))
                    (first))]
      (last xs))))
