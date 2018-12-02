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


(day2-p1 day2-data)
