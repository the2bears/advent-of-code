(ns a-o-c-2021.graph
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map priority-map-by]]))

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

(def small-data
  "123
112
341")

;;
;;  vertex  | shortest path | via vertex
;;          |   from start  |
;;----------+---------------+-----------
;;  :vertex |     :cost     |   :via
;;
;; {[1 1] {:cost 5 :via [1 0]}

(defn comparator-fn [x y]
  (let [x (x :cost)
        y (y :cost)]
    (cond (= x y) 0
          (< x y) -1
          :else 1)))

(def priority-cost-map (priority-map-by comparator-fn
                                        [1 1] {:cost 5 :via [1 0]}
                                        [1 2] {:cost 1 :via [1 0]}
                                        [2 2] {:cost 15 :via [1 0]}
                                        [2 1] {:cost 2 :via [1 0]}
                                        [3 2] {:cost 3 :via [1 0]}
                                        [4 2] {:cost 9 :via [1 0]}))

(def cost-map {[1 1] {:cost 5 :via [1 0]}
               [1 2] {:cost 1 :via [1 0]}
               [2 2] {:cost 15 :via [1 0]}
               [2 1] {:cost 2 :via [1 0]}
               [3 2] {:cost 3 :via [1 0]}
               [4 2] {:cost 9 :via [1 0]}})

(def p (into priority-cost-map cost-map))

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

(defn map-matrix
  "Takes prep-data and maps points to values (costs)"
  [c]
  (let [t-m (atom {})]
    (doseq [x (range (count (first c)))
            y (range (count c))]
      (swap! t-m assoc [x y] (get-x-y-val c x y)))
    @t-m))

(defn neighbours
  "Takes a point and returns a sequence of its neightbouring points"
  ([[x y :as p]]
   (neighbours p false))
  ([[x y] exclude-corners?]
   (for [dx [-1 0 1] dy [-1 0 1] :when (and (if exclude-corners?
                                              (or (= dx 0) (= dy 0))
                                              true))
                                 :when (not= 0 dx dy)]
     [(+ dx x) (+ dy y)])))

(defn matrix [sx sy]
  (mapv (fn[x](mapv (fn[y] (identity x)) (range sx))) (range sy)))

(defn unmap-matrix [m n]
  (let [c (matrix n n)]
    (reduce #(assoc-in %1 [(second %2) (first %2)] (get m %2))
            c
            (keys m))))

(defn map-neighbors-cost [m p]
  (let [ns (neighbours p true)]
    (->> (zipmap ns (map #(get m %) ns))
         (filter (fn[[k v]]
                   (some? v)))
         (into {}))))

(defn graph-matrix-map
  "Graph of map-matrix with costs"
  [m]
  (let [graph m
        ks (keys graph)]
    (zipmap ks (map #(map-neighbors-cost m %) ks))))


(def test-map (map-matrix (prep-data test-data)))
(def small-map (map-matrix (prep-data small-data)))

(let [p [1 0]]
  (->> (zipmap (neighbours p true) (map #(get small-map %) (neighbours p true)))
       (filter (fn[[k v]]
                 (some? v)))
       (into {})))
               

