(ns a-o-c-2021.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2021/day/15")

(def test-data
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(def sample-data
  "1111
1111
1111
1111")

(def data
  (-> (slurp (io/resource "day15.txt"))))

(defn prep-data [d]
  (-> d
      str/split-lines
      (->> (mapv #(seq %))
           (mapv #(mapv (fn[c]
                          (Integer/parseInt (str c)))
                        %)))))
(defn matrix [sx sy]
  (mapv (fn[x](mapv (fn[y] 0) (range sx))) (range sy)))

(defn get-x-y-val [c x y]
  (get (get c y) x))

(defn map-matrix [c]
  "Takes prep-data"
  (let [t-m (atom {})]
    (doseq [x (range (count (first c)))
            y (range (count c))]
      (swap! t-m assoc [x y] (get-x-y-val c x y)))
    @t-m))

(defn matrix [sx sy]
  (mapv (fn[x](mapv (fn[y] (identity x)) (range sx))) (range sy)))

(defn unmap-matrix [m n]
  (let [c (matrix n n)]
    (reduce #(assoc-in %1 [(second %2) (first %2)] (get m %2))
            c
            (keys m))))

(defn min-at-x-y [out-m in-m x y]
  (let [enter-risk (get (get in-m y) x)
        current-min (cond
                      (= 0 x y) 0
                      (= 0 y) (+ enter-risk (get (get out-m y) (- x 1)))
                      (= 0 x) (+ enter-risk (get (get out-m (- y 1)) x))
                      :else (+ enter-risk (min
                                           (get (get out-m (- y 1)) x)
                                           (get (get out-m y) (- x 1)))))]
      (assoc-in out-m [y x] current-min)))


(defn inc-matrix [m]
  (mapv #(mapv (fn[c]
                 (if (= c 9) 1 (+ c 1)))
               %)
        m))
(defn inc-row [r]
  (mapv (fn[c]
           (if (= c 9) 1 (+ c 1)))
        r))

(defn reduce-rows [r n]
  (loop [r r
         c 0
         acc r]
    (if (= c n)
      acc
      (let [next-row (inc-row r)]
       (recur next-row (+ 1 c) (into [] (concat acc next-row)))))))

(defn top-five [pd n]
  (mapv #(reduce-rows % n) pd))

(defn expand-top-five [pd n]
  (loop [m (top-five pd n)
         c 0
         acc m]
    (if (= c n)
      acc
      (let [next-rows (inc-matrix m)]
        (recur next-rows (+ 1 c) (into [] (concat acc next-rows)))))))


(def t2 (expand-top-five (prep-data test-data) 4))

