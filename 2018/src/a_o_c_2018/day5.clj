(ns a-o-c-2018.day5
  (:require [clojure.java.io :as io]))

(def day5-data (slurp "resources/day5.txt"))

(defn reactive-pair? [a b]
  (and (not= a b)
       (= (clojure.string/lower-case (str a))
          (clojure.string/lower-case (str b)))))

