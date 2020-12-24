(ns advent-of-code.solution.day-23)

(defn move
  [cups]
  (let [picked-up (take 3 (next cups))
        destination (->> (concat (take-while pos? (iterate dec (first cups)))
                                 (range 9 0 -1))
                         (drop 1)
                         (drop-while (set picked-up))
                         (first))
        next-cups (concat (drop 4 cups) (take 1 cups))
        head (take-while (complement #{destination}) next-cups)
        tail (drop-while (complement #{destination}) next-cups)]
    (vec (concat head (take 1 tail) picked-up (drop 1 tail)))))

(defn move-1M
  [cups]
  (let [picked-up (take 3 (next cups))
        destination (->> (concat (take-while pos? (iterate dec (first cups)))
                                 (range 1000000 0 -1))
                         (drop 1)
                         (drop-while (set picked-up))
                         (first))
        next-cups (concat (drop 4 cups) (take 1 cups))
        head (take-while (complement #{destination}) next-cups)
        tail (drop-while (complement #{destination}) next-cups)]
    (concat head (take 1 tail) picked-up (drop 1 tail))))

(def part-1
  (->> (iterate move (mapv #(Long/parseLong (str %))
                           (slurp "resources/input/day-23.txt")))
       (drop 100)
       (first)
       (drop 1)
       (apply str)))

(defn move-ring
  [[current ring]]
  (let [picked-up (take 3 (drop 1 (iterate ring current)))
        destination (->> (concat (take-while pos? (iterate dec current))
                                 (range 1000000 0 -1))
                         (drop 1)
                         (drop-while (set picked-up))
                         (first))
        after-dest (ring destination)
        next-current (nth (iterate ring current) 4)]
    [next-current
     (-> ring
         (assoc current next-current)
         (assoc destination (first picked-up))
         (assoc (last picked-up) after-dest))]))

(comment
  (time (->> (iterate move-1M (concat (map #(Long/parseLong (str %))
                                           (slurp "resources/input/day-23.txt"))
                                      (range 10 (inc 1000000))))
             (drop 10)
             (first)
             (take 130)))

  (->> (iterate move-1M (let [n 249997]
                          (concat '(5 1 7 2 3)
                                  (apply concat (take n (partition 3 4 (range 10 1000001))))
                                  '(9 8 6 4)
                                  (apply concat (take n (partition 1 4 (range 13 1000001)))))))
       (take 20)
       (map first))

  (+ (/ (- 999997 105) 4) 30))


(def part-2
  (->> (iterate move-ring [5 (->> (concat (map #(Long/parseLong (str %))
                                               (slurp "resources/input/day-23.txt"))
                                          (range 10 (inc 1000000)))
                                  (cycle)
                                  (partition 2 1)
                                  (take 1000000)
                                  (map vec)
                                  (into {}))])
       (drop 10000000)
       (first)
       (second)
       (#(iterate % 1))
       (take 3)
       (drop 1)
       (reduce *)))
