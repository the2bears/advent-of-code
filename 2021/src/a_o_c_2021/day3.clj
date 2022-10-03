(ns a-o-c-2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(comment "https://adventofcode.com/2021/day/3")

(def data
  (-> (slurp (io/resource "day3.txt"))
      (str/split-lines)))

(defn map-to-digits [c]
  (map #(Character/digit % 10) c))

(defn one-or-zero [c v]
  (if (< (/ c 2) v) 1 0))

(defn map-to-one-or-zero [c s]
  (map #(one-or-zero c %) s))

(defn inverse-byte [c]
  (map #(if (= 0 %) 1 0) c))

(defn part1-fn [data]
  (let [c (count data)
        data  (-> data
                  (->> (map seq)
                       (map map-to-digits)
                       (reduce #(map + %1 %2) (repeat 12 0))))
        gamma-rate (->> data
                        (map-to-one-or-zero c))
        epsilon-rate (->> (inverse-byte gamma-rate))
        gamma-rate-i (Integer/parseInt (apply str gamma-rate) 2)
        epsilon-rate-i (Integer/parseInt (apply str epsilon-rate) 2)]
    (* gamma-rate-i epsilon-rate-i)))

(def part1 (part1-fn data))

(println (str "Answer for Part 1 is " part1))
;;3901196

