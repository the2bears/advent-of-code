(ns a-o-c-2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(comment "https://adventofcode.com/2021/day/1")

(defn increasing? [t]
  (let [[l r] t]
    (when (> r l) true)))

(defn partition-and-compare [c]
  (->> c
       (partition 2 1)
       (keep increasing?)
       (count)))

(def data
  (-> (slurp (io/resource "day1.txt"))
      (str/split-lines)
      (->> (map #(Integer/parseInt %)))))
       

(def part1
  (->> data
       (partition-and-compare)))

(println (str "Answer for Part 1 is " part1))

(def part2
  (->> data
       (partition 3 1)
       (map #(apply + %))
       (partition-and-compare)))

(println (str "Answer for Part 2 is " part2))

