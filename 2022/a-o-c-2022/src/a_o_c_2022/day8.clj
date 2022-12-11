(ns a-o-c-2022.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [a-o-c-2022.graph :as graph]))


(comment "https://adventofcode.com/2022/day/8")

(def test-data
  "30373
25512
65332
33549
35390")

(def data
  (-> (slurp (io/resource "day8.txt"))))

(defn prep-data [d]
  (-> d
      str/split-lines
      (->> (mapv #(seq %))
           (mapv #(mapv (fn[c]
                          (Integer/parseInt (str c)))
                        %)))))

(defn visible? [pd [x y :as p]]
  (let [max-x (count (first pd)) 
        max-y (count pd) 
        from-right? (> (get-in pd [y x])
                       (if (= (dec max-x) x) -1
                           (apply max (for [x1 (range (inc x) max-x)]
                                        (get-in pd [y x1])))))
        from-left? (> (get-in pd [y x])
                      (if (= 0 x) -1
                          (apply max (for [x1 (range 0 x)]
                                       (get-in pd [y x1])))))
        from-bottom? (> (get-in pd [y x])
                        (if (= (dec max-y) y) -1
                            (apply max (for [y1 (range (inc y) max-y)]
                                         (get-in pd [y1 x])))))
        from-top? (> (get-in pd [y x])
                     (if (= 0 y) -1
                         (apply max (for [y1 (range 0 y)]
                                      (get-in pd [y1 x])))))]
    (or from-right? from-left? from-bottom? from-top?)))
       
(defn part1-fn [d]
  (let [pd (prep-data d)]
    (->> (for [x (range 0 (count (first pd)))
               y (range 0 (count pd))]
           (visible? pd [x y]))
         (filter #(identity %))
         count)))
    
(defn view-score [pd [x y :as p]]
  (let [max-x (count (first pd)) 
        max-y (count pd)
        p-height (get-in pd [y x])
        reduce-fn (fn[acc v]
                    (cond (>= v p-height) (reduced (inc acc))
                          :else (+ 1 acc)))
        looking-right (if (= (dec max-x) x) 0
                          (reduce reduce-fn
                                  0
                                  (map #(get-in pd [y %])
                                       (range (inc x) max-x))))
        looking-left (if (= 0 x) 0
                         (reduce reduce-fn
                                 0
                                 (map #(get-in pd [y %])
                                      (reverse (range 0 x)))))
        looking-down (if (= (dec max-y) y) 0
                         (reduce reduce-fn
                                 0
                                 (map #(get-in pd [% x])
                                      (range (inc y) max-y))))
        looking-up (if (= 0 y) 0
                       (reduce reduce-fn
                               0
                               (map #(get-in pd [% x])
                                    (reverse (range 0 y)))))]
    (apply * [looking-right looking-left looking-down looking-up])))


(defn part2-fn [d]
  (let [pd (prep-data d)]
    (->> (for [x (range 0 (count (first pd)))
               y (range 0 (count pd))]
           (view-score pd [x y]))
         (apply max))))
