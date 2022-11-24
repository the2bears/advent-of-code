(ns a-o-c-2021.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2021/day/12")

(def test-data
  "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def sample-data
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def data
  (-> (slurp (io/resource "day12.txt"))))

(defn upper-case? [s]
  (= (name s) (str/upper-case (name s))))

(defn prep-data [d]
  "Takes data and splits to keyword pairs"
  (-> d
      str/split-lines
      (->> (map #(str/split % #"-"))
           (mapv #(mapv (fn[s]
                          (keyword s))
                        %)))))

(defn add-node [g [s e]]
  (-> g
      (add-edge s e)
      (add-edge e s)))

(defn add-edge [g s e]
  "Add node tuple n to graph g"
  (let [n (get g s [])]
    (assoc g s (conj n e))))

(defn build-graph [d]
  "Takes line deliminated data d"
  (reduce #(add-node %1 %2) {} (prep-data d)))

(defn- find-paths* [graph node path visited]
  (let [possibles (into [] (filter #(not (contains? visited %)) (get graph node)))]
        ;;_ (prn possibles)]
    (if (and (not= node :end) (seq possibles))
      (let [new-visited (if (upper-case? node)
                          visited
                          (conj visited node))]
        ;;why conj nests here?
        (apply concat (mapv
                       #(find-paths* graph % (conj path %) new-visited)
                        possibles)))
      (when (= :end (last path))
        (do ;;(prn path)
            path)))))

(defn find-paths [g]
  "Takes a graph g, map of keywords to vector of endpoits"
  (find-paths* g :start [:start] #{}))

(def part1 (find-paths (build-graph sample-data)))

(defn part1-fn [d]
  (let [paths (find-paths (build-graph d))
        paths (filter #(= :start %) paths)]
    (count paths)))

(def part1 (part1-fn data))
   
(println (str "Answer for Part 1 is " part1))

                  
