(ns a-o-c-2021.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2021/day/11")

(def test-data
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def sample-data
  "11111
19991
19191
19991
11111")

(def data
  (-> (slurp (io/resource "day11.txt"))))

(defn map-to-int [c]
  (mapv #(Integer/parseInt (str %)) c))

(defn prep-data [d]
  (-> d
      str/split-lines
      (->> (mapv #(seq %))
           (mapv #(map-to-int %)))))

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

(defn add-one [m]
  (let [p (count (first m))
        c (flatten m)]
    (->> c
         (mapv (fn[n]
                 (let [n (+ 1 n)]
                   (if (> n 9) 0 n))))
         (partition p)
         (mapv #(into [] %)))))

(defn new-zeroes [m s]
  "Takes m, map-matrix, and s, set of points seen"
  (let [new-zeroes (->> m
                        (filter (fn[[k v]]
                                  (= 0 v)))
                        (into #{})
                        keys
                        (into #{}))]
    (set/difference new-zeroes s)))

(defn increment-point-map [m p]
  (let [n (get m p)
;;        _ (prn (str "n " n " p " p))
        nv (+ 1 n)
        nv (if (> nv 9) 0 nv)]
    (if (not= n 0)
      (assoc m p nv)
      m)))

(defn neighbours 
  "Determines all the neighbours of a given coordinate"
  [[x y] n]
  (for [dx [-1 0 1] dy [-1 0 1] :when (and
                                       (>= (+ x dx) 0)
                                       (>= (+ y dy) 0)
                                       (< (+ x dx) n)
                                       (< (+ y dy) n)
                                       (not= 0 dx dy))]
    [(+ dx x) (+ dy y)]))

(defn apply-flash [m p v]
  "Takes m, map-matrix, and p, point [x y], v size of matrix"
  (let [n (neighbours p v)]
    (reduce #(increment-point-map %1 %2) m n)))
  
(defn apply-zeroes [m s n]
  "Takes m, map-matrix, and s, set of points equal to zero"
  (do
    ;;(prn "apply-zeroes")
    ;;(clojure.pprint/pprint (unmap-matrix m n))
    ;;(clojure.pprint/pprint s)
    (reduce #(apply-flash %1 %2 n) m s)))

(def pd (prep-data sample-data))
(def ao (add-one pd))
(def mm (map-matrix ao))
(def nz (new-zeroes mm #{}))
(def az (apply-zeroes mm nz 5))
(def um (unmap-matrix az 5))

(defn next-gen [d n]
  "Takes prepped data"
  (let [pd d;;(prep-data d)
        ao (add-one pd)
        mm (map-matrix ao)
        nz (new-zeroes mm #{})
        az (apply-zeroes mm nz n)
        um (unmap-matrix az n)]
    (loop [nz nz
           mm az
           seen nz]
      (let [nz2 (new-zeroes mm seen)]
        (if (seq nz2)
          (recur nz2 (apply-zeroes mm nz2 n) (into seen nz2))
          (unmap-matrix mm n))))))

(defn count-flashes [d]
  (count (filter #(= 0 %) (flatten d))))

(defn part1-fn [d n i]
  (loop [d (prep-data d)
         i i
         flashes 0]
    (if (< i 0)
      flashes
      (let [ng (next-gen d n)
            cf (count-flashes d)]
        (recur ng (- i 1) (+ cf flashes))))))

(def part1 (part1-fn data 10 100))
   
(println (str "Answer for Part 1 is " part1))

(defn part2-fn [d n]
  (loop [d (prep-data d)
         i 0]
    (let [cf (count-flashes d)]
      (if (= cf (count (flatten d)))
        i
        (let [ng (next-gen d n)]
          (recur ng (+ i 1)))))))

(def part2 (part2-fn data 10))
   
(println (str "Answer for Part 2 is " part2))
