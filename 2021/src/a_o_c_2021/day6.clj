(ns a-o-c-2021.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(comment "https://adventofcode.com/2021/day/6")

(def data
  (-> (slurp (io/resource "day6.txt"))
      (str/trim)))
      ;;(str/split-lines)))

(def lantern-fish2 [3 4 3 1 2])

(def lantern-fish
  (-> data
      (str/split #",")
      (->> (map #(Integer/parseInt %)))))
                  
(defn next-generations [d l]
  (loop [d d
         acc l]
    (if (= 0 d)
      acc
      (recur (- d 1)
             (let [end (count acc)]
               (reduce #(if (= 0 (get %1 %2))
                           (conj (assoc %1 %2 6) 8)
                           (assoc %1 %2 (- (get %1 %2) 1)))
                       acc (range end)))))))

(defn next-generations-2 [d l]
  (loop [d d
         acc l]
    (if (= 0 d)
      acc
      (recur (- d 1)
             (let [zeroes (get acc 0 0)
                   data (assoc acc 0 0)
                   data2 (reduce #(-> %1
                                      (assoc (- %2 1) (+ (get %1 (- %2 1) 0) (get %1 %2 0)))
                                      (assoc %2 0))
                                 data (range 1 9))]
               (-> data2
                   (assoc 6 (+ (get data2 6 0) zeroes))
                   (assoc 8 (+ (get data2 8 0) zeroes))))))))

(defn part1-fn [l n]
  (->> l
       (into [])
       (next-generations n)
       count))
             
(def part1 (part1-fn lantern-fish 80))

(println (str "Answer for Part 1 is " part1))

(defn part2-fn [d l]
  (let [ng (next-generations-2 d (frequencies l))]
    (reduce #(+ %1 (get ng %2 0) ) 0 (range 9))))
                
(def part2 (part2-fn 256 lantern-fish))

(println (str "Answer for Part 2 is " part2))

