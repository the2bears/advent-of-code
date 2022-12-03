(ns a-o-c-2022.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2022/day/1")

(def test-data
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(def data
  (-> (slurp (io/resource "day1.txt"))))

(defn prep-data [d]
  (-> d
      str/split-lines
      (->> (partition-by #(not= 0 (count %)))
           (filter #(not= % '("")))
           (map #(map (fn[n]
                        (Integer/parseInt n))
                      %))
           (map #(apply + %)))))

(defn part1-fn [d]
  (-> d
      prep-data 
      sort
      last))

(def part1 (part1-fn data))

(defn part2-fn [d]
  (-> d
      prep-data
      sort
      reverse
      (->> (into [])
           (take 3)
           (apply +))))


(def part2 (part2-fn data))
