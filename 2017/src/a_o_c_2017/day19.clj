(ns a-o-c-2017.day19
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))


(def test-data
  "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
")

(def data
  (slurp (io/resource "day19.txt")))

(defn grid [y x v]
  [[x y] v])

(defn rows [y row]
  (map-indexed (partial grid y) row))

(defn processed-data
  "Returns a map of point [x y] to character, removing spaces"
  [data]
  (let [data (-> data
                 (str/split-lines)
                 (->> (map-indexed rows) ;data)
                      (apply concat)
                      (remove (fn[[[_ _] v]] (= \space v)))
                      (reduce (fn[acc [p v]] (assoc acc p v)) {})))]
    data))

(defn find-starting-point [data]
  (-> (filter (fn[[[_ y] v]] (and (= y 0)
                                  (= v \|)))
              data)
      first))

(defn neighbour [[x y] dir]
  (case dir
    :d [x (inc y)]
    :u [x (dec y)]
    :l [(dec x) y]
    :r [(inc x) y]))    

(defn neighbours [[x y]]
  #{[x (inc y)]
    [x (dec y)]
    [(dec x) y]
    [(inc x) y]})    

(defn opp-dir [dir]
  (case dir
    :l :r
    :r :l
    :u :d
    :d :u))

(defn next-p [data {:keys [dir point] :as m}]
  (let [point (neighbour point dir)]
    (when (data point)
      point)))

(defn dir-to [[x y] [x2 y2]]
  (cond (< x x2)
        :r
        (> x x2)
        :l
        (< y y2)
        :d
        (> y y2)
        :u))

(defn check-plus [data p cur-dir]
  (let [last-p (neighbour p (opp-dir cur-dir))
        p-set (set (keys data))
        new-nbrs (set/intersection p-set (neighbours p))
        how-many (count new-nbrs)
        next-p (when (= 2 how-many)
                 (first (set/difference new-nbrs (conj #{} last-p))))
        next-dir (if next-p
                   (dir-to p next-p)
                   :none)]
    next-dir))

(defn next-dir [data p cur-dir]
  (let [p-char (data p)]
    (cond (or (Character/isLetter p-char)
              (#{\- \|} p-char))
          cur-dir
          (= p-char \+)
          (check-plus data p cur-dir)
          :else :not-yet-implemented)))    

(defn next-state [data {:keys [dir] :as m}]
  (if-let [n-p (next-p data m)]
    (let [n-char (data n-p)
          n-dir (next-dir data n-p dir)
          result {:point n-p :n-char n-char :dir n-dir}]
        result)))

(next-state (processed-data test-data) {:dir :d :point [5 0]})

                                        ;LXWCKGRAOY is the correct answer
(defn part-1 [data]
  (let [data (processed-data data)
        start (first (find-starting-point data))
        dir :d]
    (loop [point start
           dir dir
           acc []]
      (if-not dir
        (apply str acc)
        (let [new-state (next-state data {:point point :dir dir})
              new-char (:n-char new-state)]
          (recur (:point new-state)
                 (:dir new-state)
                 (if (and new-char (Character/isLetter new-char))
                   (conj acc new-char)
                   acc)))))))


                                        ;17302 is the correct answer
(defn part-2 [data]
  (let [data (processed-data data)
        start (first (find-starting-point data))
        dir :d]
    (loop [point start
           dir dir
           count 0]
      (if-not dir
        count
        (let [new-state (next-state data {:point point :dir dir})
              new-char (:n-char new-state)]
          (recur (:point new-state)
                 (:dir new-state)
                 (inc count)))))))
