(ns a-o-c-2021.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(comment "https://adventofcode.com/2021/day/5")

(def data
  (-> (slurp (io/resource "day5.txt"))
      (str/split-lines)))

(defn straight-line? [[[x1 y1] [x2 y2]]]
  "Returns true if the line is eiher horizontal or vertical"
  (or (= x1 x2) (= y1 y2)))

(defn vertical-horizontal-or-diagonal? [[[x1 y1] [x2 y2]]]
  "Returns true if the line is horizontal, vertical, or diagonal"
  (or (= x1 x2) (= y1 y2)
      (let [x-delta (Math/abs (- x1 x2))
            y-delta (Math/abs (- y1 y2))]
        (= x-delta y-delta))))

(defn points-of-line [[[x1 y1] [x2 y2]]]
  "Returns a set containing the points of the vertical/horizontal line,
   or an empty set if the line is not one of the two."
  (cond
    (= x1 x2)
    (let [[y1 y2] (if (> y2 y1) [y1 y2] [y2 y1])]
      (reduce #(conj %1 (vector x1 %2)) [] (range y1 (+ y2 1))))
    (= y1 y2)
    (let [[x1 x2] (if (> x2 x1) [x1 x2] [x2 x1])]
      (reduce #(conj %1 (vector %2 y1)) [] (range x1 (+ x2 1))))
    (and (< x1 x2) (< y1 y2))
    (reduce #(conj %1 (vector (+ %2 x1) (+ %2 y1))) [] (range 0 (+ 1 (- x2 x1))))
    (and (< x1 x2) (> y1 y2))
    (reduce #(conj %1 (vector (+ %2 x1) (- y1 %2))) [] (range 0 (+ 1 (- x2 x1))))
    (and (> x1 x2) (< y1 y2))
    (reduce #(conj %1 (vector (- x1 %2) (+ %2 y1))) [] (range 0 (+ 1 (- x1 x2))))
    :else
    (reduce #(conj %1 (vector (- x1 %2) (- y1 %2))) [] (range 0 (+ 1 (- x1 x2))))))
  

(defn add-or-increment-point [m p]
  (let [t (get m p 0)]
    (assoc m p (+ 1 t))))

(def points
  (->> data
       (map #(str/split % #"\s+"))
       (map #(vector(first %) (last %)))
       (map (fn[v]
              (map #(str/split % #",") v)))
       flatten
       (map #(Integer/parseInt %))
       (partition 2)
       (partition 2)))

(defn count-overlaps [f points]
  (let [all-points (map points-of-line (filter f points))
        points-frequency-map
          (loop [all-points all-points
                 points-map {}]
            (if-not (seq all-points)
              points-map
              (recur (rest all-points)
                     (reduce add-or-increment-point points-map (first all-points)))))
        points-filtered
        (apply dissoc points-frequency-map (for [[k v] points-frequency-map :when (< v 2)] k))]
    (count points-filtered)))
  
(def part1 (count-overlaps straight-line? points))

(println (str "Answer for Part 1 is " part1))

(def part2 (count-overlaps vertical-horizontal-or-diagonal? points))

(println (str "Answer for Part 2 is " part2))
