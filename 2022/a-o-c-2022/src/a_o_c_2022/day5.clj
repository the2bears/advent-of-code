(ns a-o-c-2022.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [a-o-c-2022.graph :as graph]))


(comment "https://adventofcode.com/2022/day/5")

(def test-data
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def data
  (-> (slurp (io/resource "day5.txt"))))


(defn prep-data [d]
  (-> d
      str/split-lines))


(defn rotate-matrix [m]
  (let [mx (count (first m))
        my (count m)
        r (atom (graph/matrix my mx))]
    (doseq [rx (range my)
            ry (range mx)]
      (swap! r assoc-in [ry (- my 1 rx)] (graph/get-x-y-val m ry rx)))
    @r))

(defn box-stacks [pd]
  (->> pd
       (take-while #(not= 0 (count %)))
       (mapv #(into [] (seq %)))
       rotate-matrix
       (map #(apply str %))
       (filter #(not= \space (first %)))
       (map #(str/trim %))
       (map #(conj {} (vector (first %)
                              (reduce conj
                                           '()
                                           (rest %)))))
       (apply merge)))

(defn command-list [pd]
  (->> pd
       (drop-while #(not= 0 (count %)))
       rest
       (map #(str/split % #" "))
       (map #(into [] (take-nth 2 (rest %))))
       (mapv #(into {} {:count (Integer/parseInt (first %))
                        :from (first (second %)) :to (first (last %))}))))

                  
(defn move-box [bxs cmd]
  (let [from (get bxs (get cmd :from))
        to (get bxs (get cmd :to))
        i (get cmd :count)]
    (loop [c 0
           from from
           to to]
      (if (= c i)
        (assoc bxs (get cmd :from) from (get cmd :to) to)
        (recur (inc c) (pop from) (conj to (peek from)))))))

(defn part1-fn [d]
  (let [pd (prep-data d)
        bxs (box-stacks pd)
        cmds (command-list pd)]
    (-> (reduce #(move-box %1 %2) bxs cmds)
        vals
        (->> (map first)
             (apply str)))))

(defn move-box-2 [bxs cmd]
  (let [from (get bxs (get cmd :from))
        to (get bxs (get cmd :to))
        i (get cmd :count)
        mvd (take i from)]
    (assoc bxs (get cmd :from) (drop i from) (get cmd :to) (concat mvd to))))
    
(defn part2-fn [d]
  (let [pd (prep-data d)
        bxs (box-stacks pd)
        cmds (command-list pd)]
    (-> (reduce #(move-box-2 %1 %2) bxs cmds)
        vals
        (->> (map first)
             (apply str)))))
