(ns a-o-c-2021.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(comment "https://adventofcode.com/2021/day/9")

(def test-data "2199943210
3987894921
9856789892
8767896789
9899965678")

(def data
  (-> (slurp (io/resource "day9.txt"))))

(defn prep-data [d]
  (-> d
      (str/split-lines)
      (->> (map #(seq %))
           (mapv (fn[c]
                   (mapv #(Integer/parseInt (str %)) c))))))


(defn get-x-y-val [c x y]
  (get (get c y) x))

(def prepped-data (prep-data test-data))

(defn map-heights [c]
  (let [t-m (atom {})]
    (doseq [x (range (count (first c)))
            y (range (count c))]
      (swap! t-m assoc [x y] (get-x-y-val c x y)))
    @t-m))
    

(def mapped-heights (map-heights prepped-data))

(defn lowpoint-or-zero [c x y m]
  (let [p (get-x-y-val c x y)]
    (if
        (and (< p (get m [(+ x 1) y] Integer/MAX_VALUE))
             (< p (get m [(- x 1) y] Integer/MAX_VALUE))
             (< p (get m [x (+ y 1)] Integer/MAX_VALUE))
             (< p (get m [x (- y 1)] Integer/MAX_VALUE)))
      p
      -1)))
          
(defn check-lowpoints [c m]
  (loop [x 0
         y 0
         acc []
         row []]
    (cond
      (= y (count c)) acc
      (= x (count (first c))) (recur 0 (+ 1 y) (conj acc row) [])
      :else (recur (+ 1 x) y acc (conj row (lowpoint-or-zero c x y m))))))

(defn part1-fn [d]
  (let [p-d (prep-data d)
        m-h (map-heights p-d)
        lowpoints (check-lowpoints p-d m-h)]
    (->> lowpoints
         flatten
         (filter #(>= % 0))
         (map #(+ 1 %))
         (apply +))))

(def part1 (part1-fn data))
   
(println (str "Answer for Part 1 is " part1))
