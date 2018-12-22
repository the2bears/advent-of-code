(ns a-o-c-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))

(def day3-data
  (->> (io/reader "resources/day3.txt")
       (line-seq)))

(defn claimed-inches [[x y] [l h]]
  (let [tuples (for [yy (range (inc y) (+ h (inc y)))
                     xx (range (inc x) (+ l (inc x)))]
                  [xx yy])]
    (into #{} tuples)))

(value split (first day3-data))

(def is-pound (sym \#))
(def is-at (sym \@))
(def is-colon (sym \:))
(def id (<*> is-pound dec-num))
(def xy (skip-ws (sep-by (sym* \,) dec-num)))
(def lw (skip-ws (sep-by (sym* \x) dec-num)))
(def claim (<*> id (skip-ws is-at) xy (>> is-colon lw)))

(defn inches-from-claim [c]
  (let [[_ _ xy lw] (value claim c)]
    (claimed-inches xy lw)))

(defn day3-p1 [data]
  (loop [inches-claimed #{}
         inches-reclaimed #{}
         claims data]
    (if-not (seq claims)
      {:count (count inches-reclaimed) :reclaimed inches-reclaimed}
      (let [inches-from-claim' (inches-from-claim (first claims))
            inches-claimed' (s/union inches-claimed inches-from-claim')
            inches-intersect (s/intersection inches-claimed (inches-from-claim (first claims)))
            inches-reclaimed' (s/union inches-reclaimed inches-intersect)]
        (recur inches-claimed' inches-reclaimed' (rest claims))))))
;;110389

(defn ids-from-claim [c]
  (let [[id _ _ _] (value claim c)]
    (second id)))

(defn day3-p2 [data]
  (let [reclaimed (:reclaimed (day3-p1 data))]
    (loop [claims data
           inches-from-claim' (inches-from-claim (first claims))
           current-id (ids-from-claim (first claims))]
      (if (empty? (s/intersection reclaimed inches-from-claim'))
        current-id
        (let [claims' (rest claims)
              inches-from-claim' (inches-from-claim (first claims'))
              current-id' (ids-from-claim (first claims'))]
          (recur claims' inches-from-claim' current-id'))))))
;;552               
             
