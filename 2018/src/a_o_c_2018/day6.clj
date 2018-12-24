(ns a-o-c-2018.day6
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as str]))


(def day6-data
  (->> (io/reader "resources/day6.txt")
       (line-seq)))

(def test-data
  (->> (clojure.string/split-lines
         "1, 1
         1, 6
         8, 3
         3, 4
         5, 5
         8, 9")
       (map str/trim)))

(defn split-coords [c]
  (->> (str/split c #", ")
       (mapv read-string)))

(defn manhattan-dist [[^long x1 ^long y1][^long x2 ^long y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn on-which-side
  "second point with respect to first point"
  [[^long x1 ^long y1][^long x2 ^long y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (if (= (Math/abs dx) (Math/abs dy))
      ;;;It's on one of the 'slope=1/-1' lines
      (cond
        (and (> x2 x1)
             (> y2 y1))
        {:s true :e true}
        (and (> x2 x1)
             (< y2 y1))
        {:e true :n true}
        (and (< x2 x1)
             (< y2 y1))
        {:n true :w true}
        :else {:w true :s true})
      ;;;else it's just blockaded on one quadrant
      (cond
        (and (> x2 x1)
             (> (Math/abs dx) (Math/abs dy)))
        {:e true}
        (and (< x2 x1)
             (> (Math/abs dx) (Math/abs dy)))
        {:w true}
        (and (> y2 y1))
        {:s true}
        (and (> y1 y2))
        {:n true}))))

(manhattan-dist [-1 -1] [2 2])

(on-which-side [0 0] [2 -5])

(defn data->map [data]
  (->> (map-indexed (fn[i o]
                      [i (split-coords o)])
                    data)
       (into {})))

(defn blockaded-status [c-id m]
  (let [coord (get m c-id)
        others (dissoc m c-id)]
    (reduce (fn[acc p]
              (merge acc (on-which-side coord (get others p))))
            {}
            (keys others))))

(defn blockaded? [c-id m]
  (= 4 (count (blockaded-status c-id m))))

(defn points-at-radius* [[^long x1 ^long y1] ^long r]
  (if (= 0 r)
    #{[x1 y1]}
    (set (apply concat (for [^long w (range (- 0 r) r)]
                         [[(+ x1 w) (+ y1 (- r (Math/abs w)))]
                          [(+ x1 (- 0 w)) (+ y1 (- 0 (- r (Math/abs w))))]])))))

(def points-at-radius
  (memoize points-at-radius*))

(defn other-points-at-radius*
  "c' is the coord we're comparing with"
  [c' m r]
  (let [others (keys (dissoc m c'))
        others-points (reduce #(into %1 (points-at-radius (get m %2) r)) #{} others)]
    others-points))

(def other-points-at-radius
  (memoize other-points-at-radius*))

(defn area-size [m c']
  (let [p' (get m c')]
    (loop [r 0
           other-points #{}
           acc #{}]
      (let [p'-points (points-at-radius p' r)
            new-other-points (into other-points (other-points-at-radius c' m r))
            uniques (s/difference p'-points new-other-points)
            new-acc (into acc uniques)]
            ;;_ (prn :r r :count-uniques (count uniques))]
        (if (= 0 (count uniques))
          (count acc)
          (recur (inc r) new-other-points new-acc))))))

(def test-map (data->map test-data))

(defn day6-p1 [data]
  (let [data (data->map data)
        blockaded (filter #(blockaded? % data) (keys data))]
    (apply max (map (partial area-size data) blockaded))))
;;5975

(defn all-manhattan-distances [[x y] coords]
  (map (partial manhattan-dist [x y]) coords))

(defn day6-p2 [data]
  (let [data (data->map data)
        coords (vals data)
        xs (map first coords)
        ys (map second coords)
        ^long x-min (apply min xs)
        ^long x-max (apply max xs)
        ^long y-min (apply min ys)
        ^long y-max (apply max ys)
        pps   (for [x (range x-min (inc x-max))
                    y (range y-min (inc y-max))]
                [x y])]
    (->> (map #(all-manhattan-distances % pps) coords)
         (apply map +)
         (filter #(< ^long % 10000))
         (count))))
;;38670


(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true))
