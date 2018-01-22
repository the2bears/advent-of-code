(ns a-o-c-2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))

(def pipe (skip-ws (sep-by (sym \/) dec-lit)))


(value pipe "123/44")

(def data
  (slurp (io/resource "day24.txt")))

(def test-data "0/2
  2/2
  2/3
  3/4
  3/5
  0/1
  10/1
  9/10")

(defn process-data [data]
  (-> (str/split-lines data)
      (->> (map #(value pipe %)))))

(defn port-of [ports n]
  (filter #(or (= n (first %))
               (= n (second %)))
          ports))

(defn other-end [port n]
  (let [[x y] port]
    (if (= x n) y x)))

(defn permutations-2 [ports n]
  (let [possibles (port-of ports n)]
    (if (seq possibles)
      (apply concat (for [x possibles]
                      (map #(cons x %) (permutations-2 (remove #{x} ports) (other-end x n)))))
      [possibles])))


                                        ;1656 is the correct answer
(defn part-1 [data start]
  (let [ports (process-data data)]
    (->> (permutations-2 ports start) 
         (map flatten)
         (map #(apply + %))
         (apply max))))


                                        ;1642 is the correct answer
(defn part-2 [data start]
  (let [ports (process-data data)
        by-lengths (->> (permutations-2 ports start) 
                        (sort-by #(count %))
                        (group-by count))
        max-length (apply max (keys by-lengths))
        longest (get by-lengths max-length)]
    (->> longest
         (map flatten)
         (map #(apply + %))
         (apply max))))
