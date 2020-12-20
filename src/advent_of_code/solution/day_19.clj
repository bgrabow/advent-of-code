(ns advent-of-code.solution.day-19
  (:require [clojure.string :as str]
            [instaparse.core :as insta])
  (:import (instaparse.gll Failure)))

(defn valid?
  ([rules s]
   (valid? rules s 0))
  ([rules _s rule]
   (if (string? (rules rule)))))

(def part-1
  (let [[rule-strings [_ & messages]] (split-with (complement str/blank?)
                                                  (str/split-lines (slurp "resources/input/day-19.txt")))
        _rules (->> rule-strings
                    (map (fn [s]
                           (let [[_ rule-number contents] (re-find #"(\d+)\:(.+)" s)
                                 terminal-rule (re-find #"[a-z]" contents)]
                             [(Long/parseLong rule-number)
                              (or terminal-rule
                                  (map (fn [s]
                                         (map #(Long/parseLong %)
                                              (re-seq #"\d+" s)))
                                       (str/split contents #"\|")))])))
                    (into {}))
         parser (insta/parser (str/join "\n" (cons "S: 0" rule-strings)))]
    (->> (map parser messages)
         (remove #(instance? Failure %))
         count)))

(def part-2
  (let [[rule-strings [_ & messages]] (split-with (complement str/blank?)
                                                  (str/split-lines (slurp "resources/input/day-19.txt")))
        _rules (->> rule-strings
                    (map (fn [s]
                           (let [[_ rule-number contents] (re-find #"(\d+)\:(.+)" s)
                                 terminal-rule (re-find #"[a-z]" contents)]
                             [(Long/parseLong rule-number)
                              (or terminal-rule
                                  (map (fn [s]
                                         (map #(Long/parseLong %)
                                              (re-seq #"\d+" s)))
                                       (str/split contents #"\|")))])))
                    (into {}))
        parser (insta/parser (str/join "\n" (cons "S: 0" (concat rule-strings ["8: 42 | 42 8\n11: 42 31 | 42 11 31"]))))]
    (->> (map parser messages)
         (remove #(instance? Failure %))
         count)))
