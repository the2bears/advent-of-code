(ns a-o-c-2018.day11)


(def size 300)

(def input 7689)

(defn hundreds [x]
  (rem (int (/ x 100)) 10))

(defn power-level [gsn x y]
  (->> x
       (+ 10)
       (* y)
       (+ gsn)
       (* (+ x 10))
       (hundreds)
       ((fn[n] (- n 5))))) 

;;Fuel cell at  122,79, grid serial number 57: power level -5.
;;Fuel cell at 217,196, grid serial number 39: power level  0.
;;Fuel cell at 101,153, grid serial number 71: power level  4.

(defn power-grid [gsn l]
  (->> (for [y (range 1 (inc l))]
         (into [] (for [x (range 1 (inc l))]
                    (power-level gsn x y))))
       (into [])))

(defn calc-3x3 [grid x y]
  (->> (for [x' (range x (+ 3 x))
             y' (range y (+ 3 y))]
         (get-in grid [(dec y') (dec x')]))
       (apply +)))

(defn calc-XxX [grid s x y]
  (->> (for [x' (range x (+ s x))
             y' (range y (+ s y))]
         (get-in grid [(dec y') (dec x')]))
       (apply +)))

(defn calc-all-3x3 [l grid]
  (->> (for [y (range 1 (dec l))
             x (range 1 (dec l))]
         [(calc-3x3 grid x y) [x y]])))    

(defn calc-all-XxX [l s grid]
  (->> (for [y (range 1 (- l (- s 2)))
             x (range 1 (- l (- s 2)))]
         [(calc-XxX grid s x y) [x y]])))    

(defn day11-p1 [input l]
  (let [grid (power-grid input l)]
    (->> grid
         (calc-all-XxX l 3)
         (sort-by first)
         (last)
         (last))))
;;[20 37]

(def extract-score (comp first last))

(defn sum-row-column
  ([grid [x y] l]
   (grid x y l))
  ([grid x y l]
   (let [xs (for [x' (range x (+ x l 1))]
              [x' (+ y l)])
         ys (for [y' (range y (+ y l))]
              [(+ x l) y'])]
     (reduce (fn [acc [x1 y1]]
               (+ acc (get-in grid [y1 x1])))
             0 (concat xs ys)))))

(defn calc-point-squares [grid x y l]
  (let [max (count grid)]
    (loop [acc []
           t 0
           s 0]
      (if (or (= (+ x s) max)
              (= (+ y s) max))
        (->> acc
             (sort (fn[[_ a] [_  b]] (compare b a)))
             (first))
        (let [new-t (+ t (sum-row-column grid x y s))]
          (recur (conj acc [s new-t])
                 new-t
                 (inc s)))))))

(defn day11-p2 [input l]
  (let [grid (power-grid input l)]
    (let [[[x y][s t]] (->> (for [x (range 0 l)
                                  y (range 0 l)]
                              [[x y] (calc-point-squares grid x y l)])
                            (sort (fn [[[x y][s t]][[x2 y2][s2 t2]]]
                                    (compare t2 t)))
                            (first))]
      (str (inc x) "," (inc y) "," (inc s)))))
;;90,16,15
