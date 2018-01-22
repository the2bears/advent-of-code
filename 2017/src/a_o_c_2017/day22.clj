(ns a-o-c-2017.day22
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))


(def data
  (slurp (io/resource "day22.txt")))


(def test-data
  "..#
  #..
  ...")

(defn print-pattern [points size]
  (let [points (set points)]
    (doseq [y (range size) ;(- size) size)
            x (range size)] ;(- size) size)]
      (do (print (if (get points (vector x y)) "#" "."))
          (when (= x (dec size)) (print "\n"))))))

(defn- process-data [data]
  (-> (str/split-lines data)
      (->> (map #(str/trim %)))))

(defn- middle [data]
  (quot (count data) 2))

(defn- row->points [row y]
  (->> (map-indexed (fn[i v](when (= \# v)
                              (vector i y)))
                    (seq row))
       (remove nil?)))

(defn- grid->points [data]
  (let [rows (process-data data)]
    (->> (map-indexed (fn[i row]
                        (row->points row i))
                      rows)
         (apply concat)
         (set))))

(defn- next-dir [cur-dir turn]
  (case cur-dir
    :up (if (= turn :left) :left :right)
    :left (if (= turn :left) :down :up)
    :down (if (= turn :left) :right :left)
    :right (if (= turn :left) :up :down)))

(defn- next-point [[x y] dir]
  (case dir
    :up [x (dec y)];0 is top of grid
    :left [(dec x) y]
    :down [x (inc y)]
    :right [(inc x) y]))

                                        ;5462 is the correct answer
(defn part-1 [data turns]
  (let [s-points (grid->points data)
        mid-p (middle (process-data data))
        s-p (vector mid-p mid-p)
        s-d :up]
    (prn :starting-point s-p)
    (loop [points s-points
           cur-point s-p
           cur-d s-d
           infections 0
           c 0]
      ;(prn :cur-point cur-point :points points)
      (if (= c turns)
        (do (prn :total-infections infections)
            (print-pattern points 30))
        (let [infected? (get points cur-point)
              ;_ (prn :infected? (if infected? true false))
              next-grid (if infected?
                          (set/difference points (conj #{} cur-point))
                          (conj points cur-point))
              n-dir (next-dir cur-d (if infected? :right :left))
              n-point (next-point cur-point n-dir)
              infections (if-not infected?
                           (inc infections)
                           infections)]
          (recur next-grid n-point n-dir infections (inc c)))))))      

(def turn-map {:up :left
               :left :down
               :down :right
               :right :up})

(defn- turn [dir degrees]
  (let [t (/ degrees 90)]
    (reduce (fn[k _](get turn-map k)) dir (range t))))       

(defn- next-dir2 [cur-dir state]
  (case state
    \# (turn cur-dir 270)
    \W cur-dir
    \F (turn cur-dir 180)
    \. (turn cur-dir 90)))

(defn- next-points [points cur-point cur-state]
  (case cur-state
    \. (assoc points cur-point \W)
    \W (assoc points cur-point \#)
    \# (assoc points cur-point \F)
    \F (assoc points cur-point \.)))

(defn print-pattern2 [points size]
  (doseq [y (range (- size) size)
          x (range (- size) size)]
    (do (print (get points (vector x y) \.))
        (when (= x (dec size)) (print "\n")))))


                                        ;2512135 is the correct answer
(defn part-2 [data turns]
  (let [s-points (reduce #(assoc %1 %2 \#) {} (grid->points data))
        mid-p (middle (process-data data))
        s-p  (vector mid-p mid-p)
        s-d :up]
    (prn :s-p s-p)
    (prn :starting-points s-points)
    (loop [points s-points
           cur-point s-p
           cur-d :up
           infections 0
           c 0]
      ;(print-pattern2 points 12)
      ;(println)
      (if (= c turns)
        (do
          (print-pattern2 points 12)
          infections)
        (let [cur-state (get points cur-point \.)
              n-dir (next-dir2 cur-d cur-state)
              n-pt (next-point cur-point n-dir)
              n-points (next-points points cur-point cur-state)]
          (recur n-points
                 n-pt
                 n-dir
                 (if (= cur-state \W)
                   (inc infections)
                   infections)
                 (inc c)))))))
  
