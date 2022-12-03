(ns a-o-c-2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2022/day/2")

(def test-data
  "A Y
B X
C Z")

(def data
  (-> (slurp (io/resource "day2.txt"))))

(defn prep-data [d]
  (-> d
      str/split-lines
      (->> (map #(str/split % #" ")))))

(def theirs {"A" :rock "B" :paper "C" :scissors})

(def ours {"X" :rock "Y" :paper "Z" :scissors})

(def select-scores {:rock 1 :paper 2 :scissors 3})

(def result-scores {:win 6 :draw 3 :loss 0})

(def wins #{[:rock :paper] [:paper :scissors] [:scissors :rock]})

(defn map-to-play [t]
  [(get theirs (first t)) (get ours (last t))])

(defn score-game [[them us :as combo]]
  (let [result-score (cond
                       (= them us) 3
                       (contains? wins combo) 6
                       :else 0)
        select-score (get select-scores us)]
    (+ result-score select-score)))
  

(defn part1-fn [d]
  (-> d
      prep-data
      (->> (mapv #(map-to-play %))
           (mapv #(score-game %))
           (apply +))))

(def part1 (part1-fn data))


