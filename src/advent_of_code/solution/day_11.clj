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

(def final-seats (iterate-until-fixed step seats))

(def part-1
  (get (->> final-seats
            vals
            frequencies)
       \#))

(defn los-neighbor
  [seats origin direction bounds]
  (let [[[xmin xmax] [ymin ymax]] bounds
        in-bounds? (fn [[x y]]
                     (and (<= xmin x xmax)
                          (<= ymin y ymax)))]
    (->> (iterate #(mapv + direction %) origin)
         (drop 1)
         (take-while in-bounds?)
         (filter #(contains? seats %))
         first)))

(defn los-neighbors
  [seats]
  (let [bounds [[(apply min (map first (keys seats))) (apply max (map first (keys seats)))]
                [(apply min (map second (keys seats))) (apply max (map second (keys seats)))]]]
    (->> (for [seat (keys seats)]
           [seat (mapv (fn [direction]
                         (los-neighbor seats seat direction bounds))
                       [[1 0] [1 1]
                        [0 1] [-1 1]
                        [-1 0] [-1 -1]
                        [0 -1] [1 -1]])])
         (into {}))))

(defn step-2
  [seats los-neighbors]
  (->> (keys seats)
       (reduce (fn [acc seat]
                 (let [nearby-seats (frequencies (map seats (los-neighbors seat)))]
                   (assoc acc seat (cond
                                     (and (= \L (get seats seat))
                                          (zero? (get nearby-seats \# 0))) \#
                                     (and (= \# (get seats seat))
                                          (>= (get nearby-seats \# 0) 5)) \L
                                     :else (get seats seat)))))
               seats)))

(defn draw-2d
  [points fill]
  (let [[[xmin xmax] [ymin ymax]]
        [[(apply min (map first (keys points))) (apply max (map first (keys points)))]
         [(apply min (map second (keys points))) (apply max (map second (keys points)))]]]
    (vec (for [y (range ymin (inc ymax))]
           (apply str (for [x (range xmin (inc xmax))]
                        (get points [x y] fill)))))))

(def final-seats-2
  (let [los-neighbors (los-neighbors seats)]
    (iterate-until-fixed #(step-2 % los-neighbors) seats)))

(def part-2
  (-> final-seats-2
      vals
      frequencies
      (get \#)))
