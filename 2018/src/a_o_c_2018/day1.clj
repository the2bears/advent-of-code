(ns a-o-c-2018.day1
  (:require [clojure.java.io :as io]))

(def data-file "resources/day1.txt")

(defn day1 [f]
  (let [rdr (clojure.java.io/reader f)]
    (->> (line-seq rdr)
         (reduce #(+ %1 (Integer/parseInt %2)) 0))))

(def test-data
  (->> (io/reader data-file)
       (line-seq)
       (map #(Integer/parseInt %))))

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

(day1 data-file)

(day1-p2 test-data)

