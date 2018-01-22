(ns a-o-c-2017.day21
  (:require [clojure.core.matrix :as mx]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))


(def data
  (slurp (io/resource "day21.txt")))

(def test-data "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#")

(defn process-data [data]
  (str/split-lines data))

(def cell (<|> (sym \.) (sym \#)))
(def row (<+> cell cell (optional cell) (optional cell)))
(def pattern (sep-by (sym \/) row))
(def assignment (skip-ws (<+> (sym \=) (sym \>))))
(def full-line (sep-by assignment pattern))

(value pattern "....")
(value full-line  "##./##./... => .#./.#./...")
(def t-fl (value full-line  "##./##./... => .#./.#./..."))

(map #(= \# %) (apply concat (first t-fl)))

(defn row->points [row y]
  (->> (map-indexed #(when (= %2 \#) [%1 y]) row)
       (remove nil?)))

(defn pattern->points [pattern]
  (->> (map-indexed #(row->points %2 %1) pattern)
       (apply concat)
       (set)))

(defn- translate* [[x y] dx dy]
  [(+ x dx) (+ y dy)])

(defn translate [p dx dy]
  (map #(translate* % dx dy) p))

(defn- flip* [[x y] a]
  (let [d (- a x)]
    [(+ a d) y]))

(defn flip [p d]
  (let [s (seq p)]
    (->> (map #(flip* % d) s)
         (map (fn[[a b]][(int a) (int b)])))))

(defn- rotate*
  [[x y] d cx cy]
  (let [[x y] (translate* [x y] (- cx) (- cy))
        p (case d
            90 [y (- x)] 
            180 [(- x) (- y)]
            270 [(- y) x]
            [x y])]
    (translate* p cx cy)))  

(defn rotate
  ([p d]
   (rotate p d 1 1)) 
  ([p d cx cy]
   (let [s (seq p)]
     (->>(map #(rotate* % d cx cy) s)
         (map (fn[[a b]][(int a) (int b)]))))))

(defn four-turns [p cx cy]
  (->> (for [n (range 4)]
         (set (rotate p (* 90 n) cx cy)))))    

(defn list-alt-patterns
  "Given a 2x2 or 3x3 pattern returns a list of all rotation/mirroring patterns"
  [p size]
  (cond (= size 2)
        (four-turns p 0.5 0.5)
        (= size 3)
        (into (four-turns (flip p 1) 1 1) (four-turns p 1 1))))

(defn add-to-book [book line]
  (let [parsed-line (value full-line line)
        n (count (first parsed-line))
        p-alts (list-alt-patterns (pattern->points (first parsed-line)) n)
        common-value (pattern->points (second parsed-line))]
    (reduce #(assoc-in %1 [n %2] common-value) book p-alts)))

(defn split-size
  "Takes a size, and returns a vector, x groups of y. Example, 4 returns [2 2], 9 returns [3 3]"
  [n]
  (if (= 0 (mod n 2))
    [(quot n 2) 2]
    [(quot n 3) 3]))  

(defn next-size [n]
  (if (= 0 (mod n 2))
      (* 3 (quot n 2))
      (* 4 (quot n 3))))

(defn- with-in [min-x x size]
  (<= min-x x (+ min-x (- size 0.5))))

(defn filter-points [points min-x min-y size]
  (filter (fn[[x y]]
            (and (with-in min-x x size)
                 (with-in min-y y size)))
          points))

(defn print-pattern [points size]
  (let [points (set points)]
    (doseq [y (range size)
            x (range size)]
      (do (print (if (get points (vector x y)) "#" "."))
          (when (= x (dec size)) (print "\n"))))))
    
(defn pattern-transform [p dx dy size book]
  (let [translated-p (set (translate p (- (* dx size)) (- (* dy size))))
        new-size (if (= size 2) 3 4)
        match (get-in book [size translated-p])] ;(first p-alts)])
     (translate match (* new-size dx) (* new-size dy))))

(def starting-pattern (pattern->points (value pattern ".#./..#/###")))

(defn filter-corners [s]
  (let [split (split-size s)
        r (range (first split))
        d (second split)]
    (for [x r
          y r]
      [(* d x) (* d y)])))

(defn fractal [p size book]
  (let [corners (filter-corners size)]
        ;_ (prn :fractal :corners corners)]
    (for [corner corners]
      (let [split (split-size size)
            dx (/ (first corner) (second split))
            dy (/ (second corner) (second split))
            sz (/ size (first split))
            p-corner (set (filter-points p (first corner) (second corner) (second split)))
            ;_ (prn :fractal :p-corner p-corner)
            p-xform (pattern-transform p-corner
                                       dx ;(/ (first corner) (second split))
                                       dy ;(/ (second corner) (second split))
                                       sz ;(/ size (first split))
                                       book)]
            ;_ (prn p-xform)]
        p-xform))))


                                        ;171 is the correct answer
(defn part-1 [data turns]
  (let [data (process-data data)
        book (reduce add-to-book {} data)]
    (loop [s-p starting-pattern
           size 3
           c 0]
      (if (= c turns)
        (do
          (prn :c c :count (count s-p)))
          ;(print-pattern s-p size))
          ;(count s-p))
        (do (prn :c c :count (count s-p))
            ;(print-pattern s-p size)
            (recur (set (apply concat (fractal s-p size book)))
               (next-size size)
               (inc c)))))))

;~8+ seconds for 11 turns with no optimization

(defn- print-matrix [m d]
  (loop [m m
         c 1]
    (when (seq m)
      (print (if (true? (first m))
                 \# \.))
      (when (= 0 (mod c d))
        (print "\n"))
      (recur (rest m) (inc c)))))
           
(defn- rotations [p]
  (if (= 4 (count p))
    (let [[a b c d] p]
      [[a b c d]
       [c a d b]
       [d c b a]
       [b d a c]])
    (let [[a b c d e f g h i] p]
       [[a b c d e f g h i]
        [i h g f e d c b a]
        [g d a h e b i f c]
        [c f i b e h a d g]])))         

(defn- flipped-rotations [p]
  (if (= 4 (count p))
    (let [[a b c d] p
          flipped [b a d c]]
      (rotations flipped))
    (let [[a b c d e f g h i] p
          flipped [c b a f e d i h g]]
      (rotations flipped))))

(defn add-to-book-2 [book line]
  (let [parsed-line (value full-line line)
        boolean-key (map #(= \# %) (apply concat (first parsed-line)))
        alternate-keys (concat (rotations boolean-key) (flipped-rotations boolean-key))
        boolean-value (map #(= \# %) (apply concat (second parsed-line)))]
    (reduce #(assoc %1 %2 boolean-value) book alternate-keys)))

(def starting-booleans (partition 3 (map #(= \# %) (apply concat (value pattern ".#./..#/###")))))

(def grid
  (into [] (for [x (range 9)]
             (into [] (for [y (range 9)]
                        (str x \: y))))))

(take 2 (map #(partition 2 %) grid))

(defn by-two [[a b]]
  (map concat a b))
  
(defn by-three [[a b c]]
  (map concat a b c))

(defn by-four [[a b c d]]
  (map concat a b c d))

(def tt (take 3 (map #(partition 3 %) grid)))
(def tf (take 4 (map #(partition 4 %) grid)))

(def tt-by-three (by-three tt))
(def tt-by-four (by-four tf))

(defn back-to-grid [[a b c]]
  (run! (fn [a' b' c'] (prn [a' b' c'])) a b c))

;this pulls apart 2 lines into 2x2 
(def t-by-two (by-two (map #(partition 2 %) grid)))
                                        ;(by-two (map #(partition 2 %) (take 2 grid)))
;divides the whole grid into 2x2 squares - 9x9 of them
(defn divide-and-group [grid]
  (let [size (if (= 0 (mod (count grid) 2)) 2 3)]
    (loop [grid grid
           acc []]
      (if (seq grid)
        (recur (drop size grid)
               (conj acc (if (= size 2)
                           (by-two (map #(partition 2 %) (take 2 grid)))
                           (by-three (map #(partition 3 %) (take 3 grid))))))
        acc))))

(defn replace-patterns [book patterns]
  (let [size (count (first (first patterns)))]
    (loop [rows patterns
           acc []]
      (if (seq rows)
        (recur (rest rows)
               (conj acc (for [pattern (first rows)]
                           (let [rep (get book pattern) 
                                 rep (if-not rep
                                       (do
                                         (prn :no-rep pattern)
                                         (if (= 9 size)
                                             (repeat 16 false)
                                             (repeat 9 false)))
                                       rep)]
                             rep))))    
        acc))))
    
                                        ;this puts a 2x2 back together, well, 2 lines
(defn rebuild [parts]
  (let [ff (first (first parts))
        size (if (= 9 (count ff)) 3 4)]
    (loop [pp parts
           acc []]
      (if (seq pp)     
        (recur (rest pp)
          (concat acc (for [start (range size)]
                        (apply concat
                               (map #(subvec (into [] %)
                                             (* size start)
                                             (+ (* size start) size))
                                    (first pp))))))
        acc))))


                                        ;2498142 is the correct answer
(defn part-2 [data turns]
  (let [data (process-data data)
        book (reduce add-to-book-2 {} data)]
    (loop [grid starting-booleans
           c 0]
      (prn :c c :+ve-count (count (filter true? (apply concat grid))))
      (if (= turns c)
        (do (count (filter true? (apply concat grid))))
            ;(print-matrix (apply concat grid) 27))
        (recur (rebuild (replace-patterns book (divide-and-group grid)))
               (inc c))))))

