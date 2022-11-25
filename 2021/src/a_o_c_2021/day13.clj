(ns a-o-c-2021.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2021/day/13")

(def test-data
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(def data
  (-> (slurp (io/resource "day13.txt"))))

(defn matrix [sx sy]
  (mapv (fn[x](mapv (fn[y] 0) (range sx))) (range sy)))

(defn fill-matrix [m]
  (let [mx (:max_x m)
        my (:max_y m)
        points (:points m)
        matrix (atom (matrix mx my))]
    (doall (map #(swap!
                  matrix
                  assoc-in [(second %) (first %)] 1)
                points))
    (assoc m :matrix @matrix)))

(defn prep-data [d]
  (let [points (-> d
                   str/split-lines
                   (->> (take-while #(> (count %) 0))
                        (map #(str/split % #","))
                        (map #(vector (Integer/parseInt (first %))
                                      (Integer/parseInt (second %))))))
        folds (-> d
                   str/split-lines
                   (->> (drop-while #(> (count %) 0))
                        (drop 1)
                        (map #(str/split % #" "))
                        (map last)
                        (map #(str/split % #"="))
                        (map #(vector (keyword (first %))
                                      (Integer/parseInt (second %))))))
        max_x (+ 1 (reduce (fn[m [x y]]
                             (if (> x m) x m))
                           0
                           points))
        max_y (+ 1 (reduce (fn[m [x y]]
                             (if (> y m) y m))
                           0
                           points))]
    (fill-matrix{:points points :folds folds
                 :max_x max_x :max_y max_y})))

(defn get-x-y-val [c x y]
  (get (get c y) x))

(defn or-ints [a b]
  (if (or (= a 1) (= b 1))
    1
    0))

(defn y-or-matrix [m1 m2]
  (let [m1 (atom m1)
        mx (count (first m2))
        my1 (- (count @m1) (count m2))
        my2 (count @m1)
        c (- my2 (count m2))]
    (doseq [x (range mx)
            y (range my1 my2)]
      (let [top-val (get-x-y-val @m1 x y)
            bot-val (get-x-y-val m2 x (- y c))]
        (swap! m1 assoc-in [y x] (or-ints top-val bot-val))))
    @m1))
        

(defn flip-y [y m]
  (let [matrix (:matrix m)
        c (- (- (count matrix) 1) y)
        top (into [] (take y matrix))
        bot (into [] (reverse (take-last c matrix)))]
    (assoc m :matrix (if (>= (count top) (count bot))
                       (y-or-matrix top bot)
                       (y-or-matrix bot top)))))

(defn x-or-matrix [m1 m2]
  (let [m1 (atom m1)
        mx1 (- (count (first @m1)) (count (first m2)))
        mx2 (count (first @m1))
        my (count @m1)
        c (- mx2 (count (first m2)))]
    (doseq [x (range mx1 mx2)
            y (range my)]
      (let [top-val (get-x-y-val @m1 x y)
            bot-val (get-x-y-val m2 (- x c) y)]
        (swap! m1 assoc-in [y x] (or-ints top-val bot-val))))
    @m1))

(defn flip-x [x m]
  (let [matrix (:matrix m)
        c (- (- (count (first matrix)) 1) x)
        left (into [] (map #(into [] (take x %)) matrix))
        right (into [] (map #(into [] (reverse (take-last c %))) matrix))]
    (assoc m :matrix (if (>= (count (first left))
                             (count (first right)))
                       (x-or-matrix left right)
                       (x-or-matrix right left)))))


(defn part1-fn [d]
  (let [fold-functions {:y flip-y :x flip-x}
        pd (prep-data d)
        first-fold (first (:folds pd))
        ff-fn ((first first-fold) fold-functions)
        crease (second first-fold)]
    (->> pd
         (ff-fn crease)
         :matrix
         flatten
         (filter #(= 1 %))
         count)))

(def part1 (part1-fn data))

(println (str "Answer for Part 1 is " part1))

(defn part2-fn [d]
  (let [fold-functions {:y flip-y :x flip-x}
        pd (prep-data d)
        folds (:folds pd)
        res (reduce (fn[acc [f n]]
                      ((f fold-functions) n acc))
                    pd
                    folds)]
    (->> (:matrix res)
         (map #(apply str %))
         (map #(str/replace % #"0" "."))
         (map #(str/replace % #"1" "#")))))



(def part2 (part2-fn data))

