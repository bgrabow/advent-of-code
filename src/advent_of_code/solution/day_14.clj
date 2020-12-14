(ns advent-of-code.solution.day-14
  (:require [clojure.string :as str]))

(defn parse-line
  [s]
  (or (nfirst (re-seq #"(mask) = (\w+)" s))
      (nfirst (re-seq #"(mem)\[(\d+)\] = (\w+)" s))))

(defn mask
  [x mask]
  (let [x-bits (format "%036d" (BigInteger. (Long/toBinaryString (Long/parseLong x))))]
    (apply str (map (fn [x m]
                      (if (= m \X) x m))
                    x-bits
                    mask))))

(defn step
  [acc [op x y]]
  (case op
    "mask" (assoc acc :mask x)
    "mem" (assoc-in acc [:mem x] (mask y (:mask acc)))))

(def part-1
  (->> (slurp "resources/input/day-14.txt")
       (str/split-lines)
       (map parse-line)
       (reduce step {})
       (:mem)
       (vals)
       (map #(BigInteger. ^String % 2))
       (reduce +)))

(defn mask-addr
  [x mask]
  (let [x-bits (format "%036d" (BigInteger. (Long/toBinaryString (Long/parseLong x))))]
    (apply str (map (fn [x m]
                      (case m
                        \0 x
                        \1 \1
                        \X \X))
                    x-bits
                    mask))))

(defn expand-mask
  [mask]
  (reduce (fn [acc c]
            (case c
              \0 (map #(str % c) acc)
              \1 (map #(str % c) acc)
              \X (mapcat #(vector (str % 0) (str % 1)) acc)))
          [""]
          mask))

(defn step-2
  [acc [op x y]]
  (case op
    "mask" (assoc acc :mask x)
    "mem" (reduce #(assoc-in %1 [:mem %2] y) acc (expand-mask (mask-addr x (:mask acc))))))

(def part-2
  (->> (slurp "resources/input/day-14.txt")
       (str/split-lines)
       (map parse-line)
       (reduce step-2 {})
       (:mem)
       (vals)
       (map #(BigInteger. ^String %))
       (reduce +)))
