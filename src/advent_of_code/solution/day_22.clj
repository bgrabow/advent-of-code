(ns advent-of-code.solution.day-22
  (:require [clojure.string :as str]))

(defn p1-wins-round
  [[[p1-deck p1-discard] [p2-deck p2-discard]]]
  (let [[p1-discard p2-discard] [(into (vec p1-discard) [(first p1-deck) (first p2-deck)]) p2-discard]]
    [[(or (seq (next p1-deck))
          (apply list p1-discard))
      (and (seq (next p1-deck)) p1-discard)]
     [(or (seq (next p2-deck))
          (apply list p2-discard))
      (and (seq (next p2-deck)) p2-discard)]]))

(defn p2-wins-round
  [[[p1-deck p1-discard] [p2-deck p2-discard]]]
  (let [[p1-discard p2-discard] [p1-discard (into (vec p2-discard) [(first p2-deck) (first p1-deck)])]]
    [[(or (seq (next p1-deck))
          (apply list p1-discard))
      (and (seq (next p1-deck)) p1-discard)]
     [(or (seq (next p2-deck))
          (apply list p2-discard))
      (and (seq (next p2-deck)) p2-discard)]]))

(defn round
  [[[p1-deck p1-discard] [p2-deck p2-discard]]]
  (if (> (first p1-deck) (first p2-deck))
    (p1-wins-round [[p1-deck p1-discard] [p2-deck p2-discard]])
    (p2-wins-round [[p1-deck p1-discard] [p2-deck p2-discard]])))

(defn score-game
  [[[p1-deck p1-discard] [p2-deck p2-discard]]]
  (let [p1-deck (concat p1-deck p1-discard)
        p2-deck (concat p2-deck p2-discard)]
    [(reduce + (map * (range (count p1-deck) 0 -1) p1-deck))
     (reduce + (map * (range (count p2-deck) 0 -1) p2-deck))]))

(def part-1
  (let [[player-1 player-2] (map (fn [deck-string] (map #(Long/parseLong %) (str/split-lines deck-string)))
                                 (re-seq #"(?:\d+\n)+" (slurp "resources/input/day-22.txt")))]
    (->> (iterate round [[player-1 nil] [player-2 nil]])
         (drop-while (fn [[[p1-deck] [p2-deck]]]
                       (every? seq [p1-deck p2-deck])))
         (first)
         score-game
         (apply max))))

(declare recursive-game)
(declare recursive-round)

(defn normalize-state
  [[[p1-deck p1-discard] [p2-deck p2-discard]]]
  [(vec (concat p1-deck p1-discard))
   (vec (concat p2-deck p2-discard))])

(defn player-1-wins-repeat
  [states]
  (let [normalized-states (map normalize-state states)
        seen (reductions conj #{} normalized-states)]
    (map
      (fn [state seen normalized-state]
        (if (seen normalized-state)
          [(first state) nil]
          state))
      states seen normalized-states)))

(defn recursive-game
  [state]
  (if (apply < (->> (iterate recursive-round state)
                    (player-1-wins-repeat)
                    (drop-while (fn [[[p1-deck] [p2-deck]]]
                                  (every? seq [p1-deck p2-deck])))
                    (first)
                    score-game))
    :p2-wins
    :p1-wins))

(defn recursive-round
  [[[p1-deck p1-discard] [p2-deck p2-discard]]]
  (if (and (< (first p1-deck) (+ (count p1-deck) (count p1-discard)))
           (< (first p2-deck) (+ (count p2-deck) (count p2-discard))))
    (if (= :p1-wins (recursive-game [[(take (first p1-deck) (concat (next p1-deck) p1-discard)) nil]
                                     [(take (first p2-deck) (concat (next p2-deck) p2-discard)) nil]]))
      (p1-wins-round [[p1-deck p1-discard] [p2-deck p2-discard]])
      (p2-wins-round [[p1-deck p1-discard] [p2-deck p2-discard]]))
    (round [[p1-deck p1-discard] [p2-deck p2-discard]])))

(def part-2
  (let [[player-1 player-2] (map (fn [deck-string] (map #(Long/parseLong %) (str/split-lines deck-string)))
                                 (re-seq #"(?:\d+\n)+" (slurp "resources/input/day-22.txt")))]
    (->> (iterate recursive-round [[player-1 nil] [player-2 nil]])
         (drop-while (fn [[[p1-deck] [p2-deck]]]
                       (every? seq [p1-deck p2-deck])))
         (first)
         score-game
         (apply max))))
