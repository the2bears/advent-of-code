(ns a-o-c-2018.day2
  (:require [clojure.java.io :as io]))



(def day2-data
  (->> (io/reader "resources/day2.txt")
       (line-seq)))


(defn day2-p1 [data]
  (let [;;;counts map each line to the char frequencies, take just the vals (count of each char
        ;;;and into a set
        counts (->> (map frequencies data)
                    (map vals)
                    (map #(into #{} %)))
        twos (keep #(% 2) counts)
        threes (keep #(% 3) counts)]
    (* (count twos) (count threes))))


(defn same-chars
  ([[s1 s2]]
   (same-chars s1 s2)) 
  ([s1 s2]
   (loop [seq1 (seq s1)
          seq2 (seq s2)
          acc  []]
     (if (seq seq1)
       (recur (rest seq1) (rest seq2) (if-not (= (first seq1) (first seq2))
                                        acc
                                        (conj acc (first seq1))))
       (apply str acc)))))
              
(defn diff-chars
  [s1 s2]
  (loop [seq1 (seq s1)
         seq2 (seq s2)
         acc  []]
    (if (seq seq1)
      (recur (rest seq1) (rest seq2) (if (= (first seq1) (first seq2))
                                       acc
                                       (conj acc (first seq1))))
      acc)))


(day2-p1 day2-data)
;;5681

(defn diff-by-one [data]
  (for [w1 data
        w2 data
        :when (and (not= w1 w2)
                   (= 1 (count (diff-chars w1 w2))))]
    [w1 w2]))
       
(defn day2-p2 [data]
  (same-chars (first (diff-by-one data))))

(day2-p2 day2-data)
;;uqyoeizfvmbistpkgnocjtwdl
