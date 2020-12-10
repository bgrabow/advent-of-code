(ns advent-of-code.solution.day-04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-passport
  [passport]
  (into {} (for [[_ k v] (re-seq #"(\w+)\:(\#?\w+)" passport)]
             [k v])))

(def all-keys #{"hgt" "pid" "byr" "eyr" "iyr" "ecl" "cid" "hcl"})
(def required-keys #{"hgt" "pid" "byr" "eyr" "iyr" "ecl" "hcl"})

(defn missing-keys
  [passport]
  (set/difference required-keys (set (keys passport))))

(defn has-valid-keys?
  [passport]
  (-> passport
      missing-keys
      count
      (<= 0)))

(def part-1
  (-> (slurp "resources/input/day-04.txt")
      (str/split #"\n\n")
      (->> (map parse-passport)
           (filter has-valid-keys?)
           count)))

(def field-validators
  {"byr" (fn [x]
           (try
             (<= 1920 (Long/parseLong x) 2002)
             (catch NumberFormatException _ false)))
   "iyr" (fn [x]
           (try
             (<= 2010 (Long/parseLong x) 2020)
             (catch NumberFormatException _ false)))
   "eyr" (fn [x]
           (try
             (<= 2020 (Long/parseLong x) 2030)
             (catch NumberFormatException _ false)))
   "hgt" (fn [x]
           (let [[[_ n unit]] (re-seq #"(\d+)(\w+)" x)
                 bounds (case unit
                          "cm" [150 193]
                          "in" [59 76]
                          nil)]
             (when bounds
               (try
                 (<= (first bounds)
                     (Long/parseLong n)
                     (second bounds))
                 (catch NumberFormatException _ false)))))
   "hcl" (fn [x]
           (some? (re-seq #"\#[0-9a-f]{6}" x)))
   "ecl" (fn [x]
           (contains?
             #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
             x))
   "pid" (fn [x]
           (some? (re-seq #"^\d{9}$" x)))
   "cid" (constantly true)})

(defn has-valid-values?
  [m]
  (every? (fn [[k v]]
            (some->> k
                     (get field-validators)
                     ((fn [f]
                        (f v)))))
          m))

(def part-2
  (-> (slurp "resources/input/day-04.txt")
      (str/split #"\n\n")
      (->> (map parse-passport)
           (filter has-valid-keys?)
           (filter has-valid-values?)
           count)))
