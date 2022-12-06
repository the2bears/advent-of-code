(ns a-o-c-2022.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2022/day/3")

(def test-data
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def data
  (-> (slurp (io/resource "day3.txt"))))

(defn prep-data [d]
  (-> d
      str/split-lines))

(defn split-in-half [s]
  (let [half (/ (count s) 2)]
    [(take half s) (drop half s)]))

(defn priority [ch]
  (if (Character/isUpperCase ch)
    (+ 27 (- (int ch) (int \A)))
    (+ 1 (- (int ch) (int \a)))))
    
(defn part1-fn [d]
  (-> d
      prep-data
      (->> (map #(split-in-half %))
           (map #(map (fn[c]
                        (into #{} c))
                      %))
           (map #(set/intersection (first %) (second %)))
           (apply concat)
           (map priority)
           (apply +))))
                        
(def part1 (part1-fn data))

(defn part2-fn [d]
  (-> d
      prep-data
      (->> (map #(into #{} %))
           (partition 3)
           (map #(apply set/intersection %))
           (apply concat)
           (map priority)
           (apply +))))

(def part2 (part2-fn data))
