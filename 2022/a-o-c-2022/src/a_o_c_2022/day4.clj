(ns a-o-c-2022.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2022/day/4")

(def test-data
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(def data
  (-> (slurp (io/resource "day4.txt"))))

(defn vecs-to-ints [c]
  (mapv #(mapv (fn[s]
                 (Integer/parseInt s))
              %)
        c))

(defn prep-data [d]
  (-> d
      str/split-lines
      (->> (map #(str/split % #","))
           (map #(map (fn[s]
                       (str/split s #"-"))
                      %))
           (map #(vecs-to-ints %)))))

(defn total-overlap? [[[s1 e1][s2 e2]]]
  (or
    (and (<= s1 s2) (>= e1 e2)) 
    (and (<= s2 s1) (>= e2 e1)))) 

(defn part1-fn [d]
  (-> d
      prep-data
      (->> (filter #(total-overlap? %))
           count)))

(def part1 (part1-fn data))

(defn some-overlap? [[[s1 e1][s2 e2]]]
  (or
    (and (<= s1 s2) (>= e1 e2)) 
    (and (<= s2 s1) (>= e2 e1))
    (and (<= s1 s2) (>= e1 s2))
    (and (<= s2 s1) (>= e2 s1))))

(defn part2-fn [d]
  (-> d
      prep-data
      (->> (filter #(some-overlap? %))
           count)))

(def part2 (part2-fn data))
