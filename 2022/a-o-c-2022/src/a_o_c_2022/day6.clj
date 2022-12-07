(ns a-o-c-2022.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [a-o-c-2022.graph :as graph]))


(comment "https://adventofcode.com/2022/day/6")

(def test-data
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def test-data-2
  "bvwbjplbgvbhsrlpgdmjqwftvncz")

(def test-data-3
  "nppdvjthqldpwncqszvftbrmjlhg")

(def data
  (-> (slurp (io/resource "day6.txt"))))


(defn prep-data [d n]
  (->> d
       (partition n 1)))

(defn part1-fn
  ([d]
   (part1-fn d 4))
  ([d n]
   (let [pd (prep-data d n)]
     (loop [i n
            marks pd]
       (if (= (count (first marks)) (count (into #{} (first marks))))
         i
         (recur (inc i) (rest marks)))))))

(defn part2-fn [d]
  (part1-fn d 14))
