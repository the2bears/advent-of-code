(ns a-o-c-2018.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day8-data
  (-> "./resources/day8.txt"
      (slurp)
      (str/split #" ")
      (->>
        (map read-string))))

(def test-data '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(def tree-data (atom test-data))

(def drop-2 (comp rest rest))

(defn build-tree []
  (let [ch (first @tree-data)
        md (second @tree-data)
        _ (swap! tree-data drop-2)]
    {:children (reduce (fn [acc _]
                         (conj acc (build-tree)))
                       []
                       (range ch))
     :meta-data (let [i (take md @tree-data)
                      _ (dotimes [n md]
                          (swap! tree-data rest))]
                  i)}))

(defn add-meta-data [node]
  (let [s (apply + (:meta-data node))]
    (+ s (reduce #(+ %1 (add-meta-data %2)) 0 (:children node)))))             

(defn day8-p1 [data]
  (let [_ (reset! tree-data data)]
    (add-meta-data (build-tree))))
;;36027

(defn add-indexed-children [node]
  (let [ch (:children node)
        md (:meta-data node)]
    (if (empty? ch)
      (apply + md)
      (let [indexed-children (reduce #(conj %1 (get ch (dec %2)))
                                     []
                                     md)]
        (reduce #(+ %1 (add-indexed-children %2)) 0 indexed-children)))))

(defn day8-p2 [data]
  (let [_ (reset! tree-data data)]
    (add-indexed-children (build-tree))))
;;23960
