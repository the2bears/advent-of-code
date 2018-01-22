(ns a-o-c-2017.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data
  (-> (slurp (io/resource "day11.txt"))))

(def dir-map {:n 1
              :ne 2
              :se 3
              :s 4
              :sw 5
              :nw 6})

(defn- keywordize [m]
  (into {} (for [[k v] m]
             [(keyword k) v]))) 

(defn- cull-freqs [{:keys [n nw ne s sw se]
                    :or {n 0 nw 0 ne 0 s 0 sw 0 se 0}
                    :as freqs}]
  (let [hex-max (fn [a b] (max (- a b) 0))]    
    (assoc freqs
           :n (hex-max n s)
           :nw (hex-max nw se)
           :ne (hex-max ne sw)
           :s (hex-max s n)
           :sw (hex-max sw ne)
           :se (hex-max se nw))))                         

(defn- clear-zeroes [s]
  (remove #(= 0 (second (first %))) s))

(defn- sort-freqs [m]
  (let [pairs (for [[k v] m] [k v])]
    (sort-by #(dir-map (first %)) pairs)))

(defn- total-neighbours
  ([[[_ a][_ b][_ c]]]
   (let [a (or a 0)
         b (or b 0)
         c (or c 0)]
     (+ b (max a c))))
  ([[x [a b] y] flip]
   (let [flipped (vector x (vector a (- b)) y)]
         ;_ (prn flipped)]
     (total-neighbours flipped))))

(defn- massage-data [data]
  (-> data
      (str/split #"\n")
      (->> (map #(str/split % #",")))
      (flatten)))


(defn- part-1* [data]
  (let [groupings (-> data
                      keywordize
                      cull-freqs
                      sort-freqs
                      (->> (partition-by (fn[[k v]]( = 0 v))))
                      clear-zeroes)
        total-dirs (count (apply concat groupings))
        ;_ (prn :total-dirs total-dirs)
        grp-count (count groupings)
        ;_ (prn groupings)
        groupings (case grp-count
                    0 []
                    2 (concat (second groupings) (first groupings))
                    1 (seq (first groupings))
                    3 (apply concat groupings))]
        ;_ (prn :grp-count grp-count groupings)]
    (case total-dirs
      2 (max (second (first groupings)) (second (second groupings)))
      (case grp-count
        0 0
        (1 2) (total-neighbours groupings)
        3 (total-neighbours groupings true)))))


                                        ;643 is the correct answer
(defn part-1 [data]
  (part-1* (-> data
               massage-data
               frequencies)))


                                        ;1471 is the correct answer
                                        ;much faster with freq calculated ahead of time and 'dec' each recur
(defn part-2 [s]
  (let [data (reverse (massage-data s))
        freq-data (frequencies data)]
    (loop [data data
           freq-data freq-data
           acc []]
      (if-not (seq data)
        (apply max acc)
        (let [dir (first data)]
          (recur (rest data)
                 (assoc freq-data dir (dec (freq-data dir)))
                 (conj acc (part-1* freq-data))))))))

