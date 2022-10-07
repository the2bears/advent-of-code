(ns a-o-c-2021.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(comment "https://adventofcode.com/2021/day/4")

(def data
  (-> (slurp (io/resource "day4.txt"))
      (str/split-lines)))

(def called-numbers
  (-> data
      first
      (str/split #",")
      (->> (map #(Integer/parseInt %)))))


(def set-called-numbers (into #{} called-numbers))

(def bingo-cards
  (-> data
      rest
      (->> (filter #(if (> (count %) 0) %))
           (map str/trim)
           (map #(str/split % #"\s+"))
           flatten
           (map #(Integer/parseInt %))
           (partition 25)
           (map #(into [] %)))))

(defn log-row [row]
  (println row)
  row)

(defn winning-row? [card row numbers-called]
  (let [start-index (* row 5)
        row (subvec card start-index (+ start-index 5))]
    (->> row
         (map #(contains? numbers-called %))
         (reduce #(and %1 %2) true))))

(defn winning-column? [card col numbers-called]
  (let [start-index col
        col (into [] (take-nth 5 (drop start-index card)))]
    (->> col
         (map #(contains? numbers-called %))
         (reduce #(and %1 %2) true))))
                                       
(defn winner-in-rows? [card numbers-called]
  (let [numbers-called (into #{} numbers-called)]
    (->> (range 0 5)
         (map #(winning-row? card % numbers-called))
         (reduce #(or %1 %2) false))))

(defn winner-in-columns? [card numbers-called]
  (let [numbers-called (into #{} numbers-called)]
    (->> (range 0 5)
         (map #(winning-column? card % numbers-called))
         (reduce #(or %1 %2) false))))

(defn winning-card? [card numbers-called]
  (or (winner-in-rows? card numbers-called)
      (winner-in-columns? card numbers-called)))

(defn winner-in-cards? [cards numbers-called]
  (let [card-count (count cards)]
    (loop [cards cards]
      (cond
        (not (seq cards)) false
        (winning-card? (first cards) numbers-called) {:card (first cards) :index (- card-count (count cards))}
        :else (recur (rest cards))))))

(defn find-winning-card [cards numbers-called]
  (loop [call-index 5]
    (cond
      (>= call-index (count numbers-called)) "No Winner!"
      (winner-in-cards? cards (take call-index numbers-called))
      {:card-meta (winner-in-cards? cards (take call-index numbers-called)) :call-index call-index}
      :else (recur (+ 1 call-index)))))

(defn part1-solution [cards numbers-called]
  (let [winning-card-meta (find-winning-card cards numbers-called)
        cards-vec (into [] cards)
        derived-card (get cards-vec (get-in winning-card-meta [:card-meta :index]))
        call-index (winning-card-meta :call-index)
        last-called (last (take call-index numbers-called))
        card-numbers-not-called (set/difference (into #{} derived-card)
                                                (into #{} (take call-index numbers-called)))
        sum-not-called (apply + card-numbers-not-called)]
    (* sum-not-called last-called)))

(part1-solution bingo-cards called-numbers)

(defn part2-solution [cards numbers-called]
  (loop [cards-set (into #{} cards)
         last-winner {}]
    (cond
      (empty? cards-set)
      (let [cards-vec (into [] cards-set)
            derived-card (get-in last-winner [:card-meta :card])
            call-index (last-winner :call-index)
            last-called (last (take call-index numbers-called))
            card-numbers-not-called (set/difference (into #{} derived-card)
                                                    (into #{} (take call-index numbers-called)))
            sum-not-called (apply + card-numbers-not-called)]
        (* sum-not-called last-called))
      :else (let [cards-vec (into [] cards-set)
                  winning-card-meta (find-winning-card cards-vec numbers-called)
                  derived-card (get cards-vec ((winning-card-meta :card-meta) :index))]
              (recur (disj cards-set derived-card)
                     winning-card-meta)))))
    
(part2-solution bingo-cards called-numbers)
  
