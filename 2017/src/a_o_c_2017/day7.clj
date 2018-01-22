(ns a-o-c-2017.day7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))

(def data
  (slurp (io/resource "day7.data")))

(def test-data "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")


(defn process-data [data]
  (str/split-lines data))

                                        ;gynfwly is the correct answer
(defn part-1 [data]
  (let [data   (->> (process-data data)
                    (map #(str/replace % #"," ""))
                    (map #(str/split % #" "))
                    (filter #(> (count %) 2)))
        p-roots (->> data
                     (map first)
                     (set))
        all-kids (->> data
                      (map #(drop 3 %))
                      (apply concat)
                      (set))]
    (clojure.set/difference p-roots all-kids)))

(def program (<$> #(apply str %) (many1 letter)))
(def weight (skip-ws (parens dec-lit)))
(def assign (skip-ws (<+> (sym \-) (sym \>))))
(def children (skip-ws (comma-sep1 program)))
(def assignment (>> assign children))
(def full-line (<*> program weight (optional assignment)))

(value program "padx")
(value weight " (55) ")
(value assign " ->")
(value children "fsdf, afdssd, fdsdf")
(value assignment " -> fsdf, afdssd, fdsdf")
(value full-line "padx (400) -> fsdf, afdssd, fdsdf")

(defn process-data2 [data]
  (-> data
      process-data
      (->> (map #(value full-line %)))))

(defn find-assignment [data name]
  (let [nodes (filter #(= name (first %)) data)]
    (when (seq nodes)
      (first nodes))))

(defn weight
  "Returns the weight of the node, including the recursive weights of all children"
  [node]
  (cond (:children node)
        (+ (:weight node)
           (reduce (fn[acc node] (+ acc (weight node)))
                   0
                   (get node :children)))
        :else (get node :weight)))

(defn children-balanced? [node]
  (apply = (map weight (get node :children))))

(defn unbalanced-child [children]
  (->> children
       (group-by #(weight %));first
       (filter (fn[[k v]](= 1 (count v))))
       (first)
       (second)
       (first)))

(defn make-node [data name]
  (let [[name weight children] (find-assignment data name)]
    (assoc {}
           :name name
           :weight weight
           :children (map #(make-node data %) children))))

(defn find-unbalanced [node]
  (loop [parent nil
         node node]
    (if (children-balanced? node)
      [parent node]
      (recur
        node
        (unbalanced-child (get node :children))))))

(defn weights-of-children [children]
  (map #(weight %) children))

                                        ;1526 is the correct answer - "ycbgx" is 5 too heavy
(defn part-2 [data]
  (let [data (process-data2 data)
        p-roots (->> data
                     (map first)
                     (set))
        all-kids (->> data
                      (map last)
                      (apply concat)
                      (set))
        just-kids (->> data
                       (filter #(nil? (last %)))) 
        root (first (set/difference p-roots all-kids))
        tree (make-node data root)
        [parent unbalanced-node] (find-unbalanced tree)
        all-siblings-weights (weights-of-children (get parent :children))
        unbalanced-nodes-weight (weight unbalanced-node)
        target-weight (first (remove (conj #{} unbalanced-nodes-weight) all-siblings-weights))]
    {:target target-weight
     :actual unbalanced-nodes-weight
     :node-weight (:weight unbalanced-node)
     :adjusted-node-weight (+ (- target-weight unbalanced-nodes-weight) (:weight unbalanced-node))}))
