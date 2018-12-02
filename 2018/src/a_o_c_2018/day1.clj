(ns a-o-c-2018.day1
  (:require [clojure.java.io :as io]))

(def data-file "resources/day1.txt")

(def test-data
  (->> (io/reader data-file)
       (line-seq)
       (map #(Integer/parseInt %))))

(defn day1-p1 [lines]
  (reduce #(+ %1 %2) 0 lines))

(defn day1-p2 [lines]
  (loop [lines (cycle lines)
         acc #{}
         fr 0]
    (if (acc fr)
      fr
      (let [fr2 (+ fr (first lines))]
        (recur (rest lines)
               (conj acc fr)
               fr2)))))

(day1-p1 test-data)

(day1-p2 test-data)

