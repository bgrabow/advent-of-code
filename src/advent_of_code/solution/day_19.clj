(ns advent-of-code.solution.day-19
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.walk :as walk])
  (:import (instaparse.gll Failure)))

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

(defn parse-rule
  [s]
  (let [[rule-number matches] (str/split s #"\:")
        alternations (set (map #(vec (re-seq #"\b(?:\d+|\w)\b" %)) (str/split matches #"\|")))]
    [rule-number alternations]))

(def lowercase-letters (set (map char (range (int \a) (inc (int \z))))))

(defn simplified?
  [alternations]
  (not (re-find #"\d" (str alternations))))

(defn simplify
  ([{:keys [simplified unsimplified]}]
   (let [substituted (map (fn [[rule-number alternations]]
                            [rule-number (walk/postwalk #(or (simplified %) %)
                                                        alternations)])
                          unsimplified)]
     {:simplified (into simplified (filter simplified? substituted))
      :unsimplified (remove simplified? substituted)}))
  ([root rules]
   (walk/prewalk #(or (get rules %) %) (get rules root))))

(defn iterate-until-fixed
  [f x]
  (reduce (fn [s x]
            (if (s x) (reduced x) (conj s x)))
          #{} (iterate f x)))

(defn nested-coll?
  [coll]
  (and (seq coll)
       (some coll? coll)))

(defn concat-sets
  [node]
  (and (vector? node)
       (every? (fn [x]
                 (or (string? x)
                     (and (set? x)
                          (not (nested-coll? x)))))
               node)
       (set (reduce (fn [ss x]
                      (if (set? x)
                        (mapcat (fn [s]
                                  (for [x x]
                                    (str s x)))
                                ss)
                        (for [s ss]
                          (str s x))))
                    [""] node))))

(defn combine-nested-sets
  [node]
  (and (set? node)
       (every? (fn [x]
                 (or (string? x)
                     (set? x))) node)
       (reduce (fn [s x]
                 (if (set? x)
                   (into s x)
                   (conj s x)))
               #{} node)))

(defn reduce-nesting
  [t]
  (walk/postwalk #(or (and (vector? %)
                         (every? string? %)
                         (str/join %))
                      (and (or (set? %) (vector? %) (list? %))
                           (= 1 (count %))
                           (first %))
                      (concat-sets %)
                      (combine-nested-sets %)
                      %)
                 t))

(def part-2-not-invented-here
  (let [[rules [_ & messages]] (split-with (complement str/blank?)
                                           (str/split-lines (slurp "resources/input/day-19.txt")))
        compiled-rule (->> (map parse-rule rules)
                           (into {})
                           (simplify "0")
                           (iterate-until-fixed reduce-nesting))]
    (count (filter compiled-rule messages))))
