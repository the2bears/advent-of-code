(ns a-o-c-2021.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(comment "https://adventofcode.com/2021/day/8")

(def test-data
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def data
  (-> (slurp (io/resource "day8.txt"))))


(defn prep-data [d]
  (-> d
      (str/split-lines)
      (->> (map #(str/split % #"\|"))
           flatten
           rest
           (take-nth 2)
           (str/join " "))
      (str/split #" ")))

(def digits-1478 #{2 4 3 7})

(defn part1-fn [d s]
  (let [words (prep-data d)]
    (-> (filter #(contains? s (count %)) words)
        count)))

(def part1 (part1-fn data digits-1478))
                   
(println (str "Answer for Part 1 is " part1))

(defn str-sets [s]
  (->> s
       (filter #(< 0 (count %)))
       (map #(into #{} (seq %)))))


(defn prep-line [c]
  (let [l (->> c
               (map (fn [s]
                      (-> s
                          (str/split #" ")
                          str-sets))))]
    {:left (first l) :right (second l)}))


(defn prep-data-2 [d]
  (-> d
      (str/split-lines)
      (->> (map #(str/split % #"\|"))
           (map prep-line))))

(def single-line-data "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn add-singletons [m]
  (let [l (:left m)]
    (-> m
        (assoc :one (first (filter #(= 2 (count %)) l)))
        (assoc :four (first (filter #(= 4 (count %)) l)))
        (assoc :seven (first (filter #(= 3 (count %)) l)))
        (assoc :eight (first (filter #(= 7 (count %)) l))))))

(defn add-groups [m]
  (let [l (:left m)]
    (-> m
        (assoc :fives (filter #(= 5 (count %)) l))
        (assoc :sixes (filter #(= 6 (count %)) l)))))

(defn derive-nine [m]
  (let [four (:four m)
        sixes (:sixes m)
        nine (first (filter #(set/subset? four %) sixes))]
    (assoc m :nine nine)))

(defn derive-three [m]
  (let [one (:one m)
        fives (:fives m)
        three (first (filter #(set/subset? one %) fives))]
    (assoc m :three three)))

(defn derive-five [m]
  (let [one (:one m)
        nine (:nine m)
        fives (:fives m)
        five  (first (filter #(= (set/union % one) nine) fives))]
    (assoc m :five five)))

(defn derive-two [m]
  (let [three (:three m)
        five (:five m)
        fives (:fives m)
        two  (first (filter #(and (not= % five)
                                  (not= % three))
                            fives))]
    (assoc m :two two)))

(defn derive-six [m]
  (let [five (:five m)
        nine (:nine m)
        sixes (:sixes m)
        six  (->> sixes
                  (filter #(= (set/union % five) %))
                  (filter #(not= % nine))
                  first)]
    (assoc m :six six)))

(defn derive-zero [m]
  (let [six (:six m)
        nine (:nine m)
        sixes (:sixes m)
        zero  (first (filter #(and (not= % six)
                                   (not= % nine))
                             sixes))]
    (assoc m :zero zero)))

(defn drop-reduncants [m]
  (dissoc m :fives :sixes :left))

(defn sets->int [m]
  (-> m
      (assoc (:zero m) 0)
      (assoc (:one m) 1)
      (assoc (:two m) 2)
      (assoc (:three m) 3)
      (assoc (:four m) 4)
      (assoc (:five m) 5)
      (assoc (:six m) 6)
      (assoc (:seven m) 7)
      (assoc (:eight m) 8)
      (assoc (:nine m) 9)))

(defn output->int [m]
  (let [output (:right m)
        output-ints (map #(get m %) output)
        output-value (reduce (fn[acc v]
                               (+ v (* acc 10))) 
                             0 output-ints)]
    (assoc m :output output-ints :output-int output-value)))

(defn part2-fn [d]
  (let [prepped (prep-data-2 d)
        all-added (->> prepped
                       (map #(add-singletons %))
                       (map #(add-groups %))
                       (map #(derive-nine %))
                       (map #(derive-three %))
                       (map #(derive-five %))
                       (map #(derive-two %))
                       (map #(derive-six %))
                       (map #(derive-zero %))
                       (map #(drop-reduncants %)))
        int-values-added (->> all-added
                              (map #(sets->int %))
                              (map #(output->int %)))
        all-outputs (map #(get % :output-int) int-values-added)]         
    (apply + all-outputs)))

(def part2 (part2-fn data))
                   
(println (str "Answer for Part 2 is " part2))
