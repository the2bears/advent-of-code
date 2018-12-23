(ns a-o-c-2018.day5
  (:require [clojure.java.io :as io]))

;;drop the \newline 
(def day5-data (drop-last (slurp "resources/day5.txt")))

(def test-data "dabAcCaCBAcCcaDA")

(defn reactive-pair?
  [[a b]]
  (and (not= a b)
       (= (clojure.string/lower-case (str a))
          (clojure.string/lower-case (str b)))))

(count (remove reactive-pair? (partition 2 day5-data)))

(defn day5-p1 [data]
  (let [c0 (count data)
        pass1 (->> data
                   (partition-all 2)
                   (remove reactive-pair?)
                   (flatten)
                   (apply str))
        c1 (count pass1)
        pass2 (->> (rest pass1)
                   (partition-all 2)
                   (remove reactive-pair?)
                   (flatten)
                   (apply str (first pass1)))
        c2 (count pass2)]
    (if (= c0 c1 c2)
      c2
      (recur pass2))))

(day5-p1 day5-data)
;;10766
