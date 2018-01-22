(ns a-o-c-2017.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- split-nodes [s]
  (->> (str/split s #",")
       (map #(str/trim %))))

(defn edges [acc [node nodes]]
  (let [node-set (split-nodes nodes)
        edges (get acc node #{})
        new-edges (reduce #(conj %1 %2) edges node-set)
        new-acc (assoc acc node new-edges)]
    (reduce (fn [acc n]
              (assoc acc n (conj (get acc n #{}) node)))
            new-acc
            node-set)))    
            
(def data
  (-> (slurp (io/resource "day12.txt"))
      (str/split-lines)
      (->> (map #(str/split % #"<->")))
      (->> (map (fn[[a b]] (vector (str/trim a) (str/trim b)))))))          


(defn- count-traversal [node-rel starting-node]
  (loop [visited #{starting-node}
         to-visit (into [] (node-rel starting-node))]
    (if-not (seq to-visit)
      (count visited)
      (let [current-node (first to-visit)
            new-to-visit (if (visited current-node)
                           (rest to-visit)
                           (concat (rest to-visit) (node-rel current-node)))]
        (recur (conj visited current-node) new-to-visit)))))

                                        ;145 is the correct answer
(defn part-1 []
  (let [node-rel (reduce edges {} data)]
    (count-traversal node-rel "0")))

(defn- traversal-nodes [node-rel starting-node]
  (loop [visited #{starting-node}
         to-visit (into [] (node-rel starting-node))]
    (if-not (seq to-visit)
      visited
      (let [current-node (first to-visit)
            new-to-visit (if (visited current-node)
                           (rest to-visit)
                           (concat (rest to-visit) (node-rel current-node)))]
        (recur (conj visited current-node) new-to-visit)))))


                                        ;207 is the correct answer
(defn part-2 []
  (let [node-rel (reduce edges {} data)]
    (loop [count 0
           node-rel node-rel
           nodes-to-delete (traversal-nodes node-rel "0")]
      (if-not (empty? node-rel)
        (let [new-node-rel (reduce #(dissoc %1 %2) node-rel nodes-to-delete)
              new-nodes-to-delete (traversal-nodes new-node-rel (first (first new-node-rel)))]
          (recur (inc count) new-node-rel new-nodes-to-delete))
        count))))



