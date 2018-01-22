(ns a-o-c-2017.day14
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [a-o-c-2017.day10 :as day10]))

(def data "jzgqcdpd")
(def test-data "flqrgnkx")

(defn to-bin [i]
  (let [hex-str (Integer/toBinaryString i)
        padding (- 8 (count hex-str))
        padding-str (apply str (take padding (repeatedly (fn[]"0"))))]    
    (str padding-str hex-str)))

(defn part-1* [data]
  (let [lengths (concat (day10/to-ascii data) day10/std-suffix)
        circle (range 256)
        circle' (day10/p1-n-times circle lengths 64)
        frq (->> circle'
                 (partition 16)
                 (map #(apply bit-xor %))
                 (map to-bin) ;day10/to-hex)
                 (apply str)
                 (frequencies))]
    ;(prn data  frq)
    (get frq \1)))

(defn part-1 [data]
  (reduce (fn [acc i] (+ acc (part-1* (str data "-" i)))) 0 (range 128)))

(defn part-2* [data]
  (let [lengths (concat (day10/to-ascii data) day10/std-suffix)
        circle (range 256)
        circle' (day10/p1-n-times circle lengths 64)
        bin-128 (->> circle'
                     (partition 16)
                     (map #(apply bit-xor %))
                     (map to-bin) ;day10/to-hex)
                     (apply str))]
     bin-128))

(defn line->coords [row-num row]
  (loop [acc #{}
         i 0
         row row]
    (if (seq row)
      (recur (if (= \1 (first row))
               (conj acc [i row-num])
               acc)
             (inc i)
             (rest row))
      acc)))

(defn neighbors [[x y]]
  #{[(inc x) y]
    [(dec x) y]
    [x (inc y)]
    [x (dec y)]})               

(defn find-island [coords]
  (let [starting-point (first coords)]
    (loop [seen #{}
           to-visit (vector starting-point)
           coords coords]
      ;(prn :seen seen :to-visit to-visit :coords coords)
      (if-not (seq to-visit)
        coords
        (let [nbrs (neighbors (first to-visit))
       ;       _ (prn :nbrs nbrs)
              new-seen (if (coords (first to-visit))
                         (conj seen (first to-visit))
                         seen)
        ;      _ (prn :new-seen new-seen)
              new-to-visit (reduce #(if (and (coords %2)
                                             (not (seen %2)))
                                      (conj %1 %2)
                                      %1)
                                   (rest to-visit) nbrs)
         ;     _ (prn :new-to-visit new-to-visit)
              new-coords (if (coords (first to-visit))
                           (set/difference coords (conj #{} (first to-visit)))
                           coords)]
          ;    _ (prn :new-coords new-coords)]
          (recur new-seen new-to-visit new-coords))))))
      

(def coords #{[1 1][1 2][3 3][3 4][3 5]})

(find-island coords)
(def set-first (conj #{} (first coords)))
(if (coords (first coords)) true false)
(set/difference coords set-first)

                                        ;1212 is the correct answer
(defn part-2 [data]
  (let [grid-strs (for [i (range 128)]
                    (part-2* (str data "-" i)))
        coords      (loop [row-num 0
                           acc #{}
                           rows grid-strs]
                      (if (seq rows)
                        (recur (inc row-num)
                               (into acc (line->coords row-num (first rows)))
                               (rest rows))
                        acc))
        _ (prn :coords (count coords))]    
    (loop [coords coords
           islands 0]
      (if-not (empty? coords)
        (recur (find-island coords) (inc islands))
        islands))))       








