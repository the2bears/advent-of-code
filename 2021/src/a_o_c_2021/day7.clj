(ns a-o-c-2021.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(comment "https://adventofcode.com/2021/day/7")

(def data
  (-> (slurp (io/resource "day7.txt"))
      (str/trim)
      (str/split #",")
      (->> (map #(Integer/parseInt %)))))

(def test-data '(16,1,2,0,4,2,7,1,2,14))

(defn total-distance [c step]
  (->> (map #(Math/abs (- % step)) c)
       (apply +)))

(defn total-distance-2 [c step]
  (->> (map #(Math/abs (- % step)) c)
       (map #(apply + (range (+ 1 %))))
       (apply +)))
   

(total-distance-2 test-data 5)

(defn all-totals-idexed [f c]
  (map-indexed #(hash-map (f c %2) %1) (range 0 (+ 1 (count c)))))

(defn sorted-totals [m]
  (into (sorted-map-by (fn [v1 v2]
                          (compare [(get m v1) v1]
                                   [(get m v2) v2])))
        m))

(defn part1-fn [c]
  (-> (all-totals-idexed total-distance c)
      (sorted-totals)
      (first)
      (first)))


(def part1 (part1-fn data))

(println (str "Answer for Part 1 is " part1))

(defn part2-fn [c]
  (-> (all-totals-idexed total-distance-2 c)
      (sorted-totals)
      (first)
      (first)))

(def part2 (part2-fn data))

(println (str "Answer for Part 2 is " part2))
