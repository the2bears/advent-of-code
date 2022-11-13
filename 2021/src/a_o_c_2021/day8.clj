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
      ;;rest
      ;;(->> (take-nth 2)
      ;;     (str/join " "))
      ;;(str/split #" ")))

(def digits-1478 #{2 4 3 7})

(defn part1-fn [d s]
  (let [words (prep-data d)]
    (-> (filter #(contains? s (count %)) words)
        count)))

(def part1 (part1-fn data digits-1478))
                   
(println (str "Answer for Part 1 is " part1))
