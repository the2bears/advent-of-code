(ns a-o-c-2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(comment "https://adventofcode.com/2021/day/2")

(defn to-command-tuple [s]
  (let [s (str/split s #" ")
        command (first s)
        d (Integer/parseInt (last s))]
    (vector command d)))

(defn split-and-evaluate [c]
  (map to-command-tuple c))

(defn update-pos [[h d][command x]]
  (case command
    "forward" (vector (+ h x) d)
    "down" (vector h (+ d x))
    "up" (vector h (- d x))))

(def data
  (-> (slurp (io/resource "day2.txt"))
      (str/split-lines)))

(def part1
  (-> data
      split-and-evaluate
      (->> (reduce update-pos [0 0])
           (#(* (first %) (second %))))))

(println (str "Answer for Part 1 is " part1))

(defn update-pos-part2 [[a h d][command x]]
 (case command
   "forward" (vector a (+ h x) (+ d (* a x)))
   "down" (vector (+ a x) h d)
   "up" (vector (- a x) h d)))

(def part2
  (-> data
      split-and-evaluate
      (->> (reduce update-pos-part2 [0 0 0])
           (#(* (second %) (last %))))))
  
(println (str "Answer for Part 2 is " part2))




