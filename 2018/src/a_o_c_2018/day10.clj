(ns a-o-c-2018.day10
  (:require [clojure.java.io :as io]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))

(def test "position=< 32017, -10464> velocity=<-3,  1>")

(def day10-data
  (->> (io/reader "resources/day10.txt")
       (line-seq)))

(def test-data
  (->> "position=< 9,  1> velocity=< 0,  2>
        position=< 7,  0> velocity=<-1,  0>
        position=< 3, -2> velocity=<-1,  1>
        position=< 6, 10> velocity=<-2, -1>
        position=< 2, -4> velocity=< 2,  2>
        position=<-6, 10> velocity=< 2, -2>
        position=< 1,  8> velocity=< 1, -1>
        position=< 1,  7> velocity=< 1,  0>
        position=<-3, 11> velocity=< 1, -2>
        position=< 7,  6> velocity=<-1, -1>
        position=<-2,  3> velocity=< 1,  0>
        position=<-4,  3> velocity=< 2,  0>
        position=<10, -3> velocity=<-1,  1>
        position=< 5, 11> velocity=< 1, -2>
        position=< 4,  7> velocity=< 0, -1>
        position=< 8, -2> velocity=< 0,  1>
        position=<15,  0> velocity=<-2,  0>
        position=< 1,  6> velocity=< 1,  0>
        position=< 8,  9> velocity=< 0, -1>
        position=< 3,  3> velocity=<-1,  1>
        position=< 0,  5> velocity=< 0, -1>
        position=<-2,  2> velocity=< 2,  0>
        position=< 5, -2> velocity=< 1,  2>
        position=< 1,  4> velocity=< 2,  1>
        position=<-2,  7> velocity=< 2, -2>
        position=< 3,  6> velocity=<-1, -1>
        position=< 5,  0> velocity=< 1,  0>
        position=<-6,  0> velocity=< 2,  0>
        position=< 5,  9> velocity=< 1, -2>
        position=<14,  7> velocity=<-2,  0>
        position=<-3,  6> velocity=< 2, -1>"
       (clojure.string/split-lines)
       (map clojure.string/trim)))

(def lt (token "=<"))
(def gt (sym \>))
(def dash (sym \-))

(def coord (<*>  (>> lt (skip-ws dec-lit)) (<< (>> comma (skip-ws dec-lit)) gt)))

(def position (token "position"))
(def velocity (token "velocity"))

(def line (<*> position coord (skip-ws velocity) coord))

(defn point [[_ [x y] _ [dx dy]]]
  {:x x
   :y y
   :dx dx
   :dy dy})

(value line test)

(defn parse-points [data]
  (reduce #(conj %1 (point (value line %2))) [] data))

(defn update-point [{:keys [dx dy] :as p}]
  (-> p
      (update :x + dx)
      (update :y + dy)))

(defn max-y-spread [points]
  (let [y-vals (map :y points)
        y-min (apply min y-vals)
        y-max (apply max y-vals)]
    (- y-max y-min)))

(defn dim-spread [k points]
  (let [k-vals (map k points)
        k-min (apply min k-vals)
        k-max (apply max k-vals)]
    [k-min k-max]))

(defn extract-points [points]
  (reduce (fn[acc p]
            (conj acc [(:x p) (:y p)]))
          #{}
          points))

(defn print-points [[x1 x2] [y1 y2] point-set]
  (doseq [y (range y1 (inc y2))]
    (do
      (doseq [x (range x1 (inc x2))]
        (print (if (point-set (vector x y))
                 \# \.)))
      (println))))  

(defn day10-p1 [data]
  (let [points (parse-points data)]
    (loop [points points
           spread (max-y-spread points)]
      (let [new-points (map update-point points)
            new-spread (max-y-spread new-points)]
        (if (> new-spread spread)
          (print-points (dim-spread :x points)
                        (dim-spread :y points)
                        (extract-points points))
          (recur new-points new-spread))))))

;;LRCXFXRP
"#.......#####....####...#....#..######..#....#..#####...#####.
 #.......#....#..#....#..#....#..#.......#....#..#....#..#....#
 #.......#....#..#........#..#...#........#..#...#....#..#....#
 #.......#....#..#........#..#...#........#..#...#....#..#....#
 #.......#####...#.........##....#####.....##....#####...#####.
 #.......#..#....#.........##....#.........##....#..#....#.....
 #.......#...#...#........#..#...#........#..#...#...#...#.....
 #.......#...#...#........#..#...#........#..#...#...#...#.....
 #.......#....#..#....#..#....#..#.......#....#..#....#..#.....
 ######..#....#...####...#....#..#.......#....#..#....#..#....."

(defn day10-p2 [data]
  (let [points (parse-points data)]
    (loop [points points
           spread (max-y-spread points)
           ticks 0]
      (let [new-points (map update-point points)
            new-spread (max-y-spread new-points)]
        (if (> new-spread spread)
          ticks
          (recur new-points new-spread (inc ticks)))))))
;;10530
