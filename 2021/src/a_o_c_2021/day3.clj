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

(defn string-to-digit-seq [c]
  (->> c
       (map seq)
       (map map-to-digits)))

(comment
  (defn >= [a b]
    (or (= a b) (> a b))))

;;k-fn is the 'keep function'. > means you keep when 1 is > c
;; < means you keep when 1 is < c
(defn filter-nth-digit [k-fn i data]
  (let [c (count data)
        one-count (reduce #(+ %1 (nth %2 i)) 0 data)
        filter-digit (if (k-fn one-count (/ c 2)) 1 0)
        filtered-data (filter #(= (nth % i) filter-digit) data)]        
    filtered-data))


(defn find-life-support-rating [k-fn s]
  (let [c (count (first s))
        data (string-to-digit-seq s)]
    (loop [i 0
           rating-data data]
      (if (or (= (count rating-data) 1) (>= i c))
        (flatten rating-data)
        (recur (+ i 1) (filter-nth-digit k-fn i rating-data))))))

(def part2
  (let [oxygen-generator-rating (find-life-support-rating >= data)
        CO2-scrubber-rating (find-life-support-rating < data)
        ogr-i (Integer/parseInt (apply str oxygen-generator-rating) 2)
        csr-i (Integer/parseInt (apply str CO2-scrubber-rating) 2)]
    (* ogr-i csr-i)))
        
(println (str "Answer for Part 2 is " part2))
