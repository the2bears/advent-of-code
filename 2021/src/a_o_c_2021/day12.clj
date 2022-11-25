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

(defn end-node? [n]
  (= :end n))

(defn prep-data [d]
  "Takes data and splits to keyword pairs"
  (-> d
      str/split-lines
      (->> (map #(str/split % #"-"))
           (mapv #(mapv (fn[s]
                          (keyword s))
                        %)))))

(defn add-edge [g s e]
  "Add node tuple n to graph g"
  (let [n (get g s [])]
    (assoc g s (conj n e))))

(defn add-node [g [s e]]
  (-> g
      (add-edge s e)
      (add-edge e s)))

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

(defn not-start? [n]
  (not= :start n))

(defn visitable? [visited node]
  ((some-fn upper-case? end-node? (complement visited)) node))
  
(def part1 (find-paths (build-graph sample-data)))

(defn part1-fn [d]
  (let [paths (find-paths (build-graph d))
        paths (filter #(= :start %) paths)]
    (count paths)))

(def part1 (part1-fn data))
   
(println (str "Answer for Part 1 is " part1))

(defn no-double-lowers [path]
  "Also ignores :start and :end"
  (->> path
       (filter #(not (upper-case? %)))
       (filter #(and (not= % :start) (not= % :end)))
       frequencies
       vals
       (filter #(> % 1))
       empty?))

(defn- find-paths-2* [graph node path visited]
  (let [possibles (into [] (filter #(not (contains? visited %)) (get graph node)))]
        ;;_ (prn possibles)]
    (if (and (not= node :end) (seq possibles))
      (let [new-visited (if (or (not (no-double-lowers path)) (upper-case? node))
                          visited
                          (conj visited node))]
        ;;why conj nests here?
        (apply concat (mapv)
           #(find-paths-2* graph % (conj path %) new-visited
                        possibles)))
      (when (= :end (last path))
         (do ;;(prn path)
             path)))))

(defn find-paths-2 [g]
  "Takes a graph g, map of keywords to vector of endpoits"
  (find-paths-2* g :start [:start] #{}))

(defn new-find-paths
  ([graph]
   (new-find-paths graph [:start] #{}))
  
  ([graph path visited]
   (let [node (last path)]
         ;;_ (prn "path " path  " visited " visited)]
     (if
       (= node :end)
       [path]
       (let [possibles (get graph node)
             possibles (filter #(visitable? visited %) possibles)
             possibles (filter #(not-start? %) possibles)]
         (mapcat (fn[n]
                   (new-find-paths graph (conj path n) (conj visited n)))
              possibles))))))

(def new-part1 (-> (new-find-paths (build-graph data))
                   count))
   
(println (str "Answer for new Part 1 is " new-part1))

(defn visitable-2? [path node]
  "Also ignores :start and :end"
  (let [lowers (->> path
                    (filter #(not (upper-case? %)))
                    (filter #(and (not= % :start) (not= % :end))))
        has-double? (not= (count lowers) (count (distinct lowers)))
        node-visited-already? (not (empty? (filter #(= % node) path)))]
    (cond
      (not has-double?) true
      (upper-case? node) true
      (and has-double? (not node-visited-already?)) true
      :else false)))

(defn new-find-paths-2
  ([graph]
   (new-find-paths-2 graph [:start] #{}))
  
  ([graph path visited]
   (let [node (last path)]
         ;;_ (prn "path " path  " visited " visited)]
     (if
       (= node :end)
       [path]
       (let [possibles (get graph node)
             possibles (filter #(visitable-2? path %) possibles)
             possibles (filter #(not-start? %) possibles)]
         (mapcat (fn[n]
                   (new-find-paths-2 graph (conj path n) (conj visited n)))
              possibles))))))


(def new-part2 (count (new-find-paths-2 (build-graph data))))

(println (str "Answer for new Part 2 is " new-part2))
