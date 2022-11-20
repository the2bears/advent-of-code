(ns a-o-c-2021.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(comment "https://adventofcode.com/2021/day/10")

(def test-data "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def data
  (-> (slurp (io/resource "day10.txt"))))


(def open-brackets #{\{ \[ \( \<})
(def close-brackets #{\} \] \) \>})
(def open->close {\{ \} \[ \] \( \) \< \>})
(def syntax-error-scores {\} 1197 \] 57 \) 3 \> 25137})
(def autocomplete-scores {\) 1 \] 2 \} 3 \> 4})


(defn prep-data [d]
  (-> d
      (str/split-lines)
      (->> (map #(seq %)))))

(def prepped-test-data (prep-data test-data))

(defn validate-line [d]
  "Takes a single char seq"
  (let [d (into [] d)]
        ;;_ (prn (apply str d))]
    (loop [stack []
           d d]
      (let [ch (first d)]
        (cond
          (contains? open-brackets ch)
          (let [new-stack (conj stack ch)]
                ;;_ (prn (apply str new-stack))]
            (recur new-stack
                   (rest d)))
          (contains? close-brackets ch)
          (let [top (peek stack)
                expected (get open->close top)]
                ;;_ (prn (str "top " top " expected " expected " got " ch))]
                ;;_ (prn (apply str (pop stack)))]
            (if (not= expected ch)
              ch
              (recur (pop stack)
                     (rest d))))
          :default
          stack)))))
        


(defn part1-fn [d]
  "Takes prep-data"
  (->> (map validate-line d)
       (filter #(contains? close-brackets %))
       (map #(get syntax-error-scores %))
       (apply +)))

(def part1 (part1-fn (prep-data data)))
   
(println (str "Answer for Part 1 is " part1))

(defn map-open-to-close [c]
  (->> c
       (map #(get open->close %))
       reverse))

(defn score-autocomplete [c]
  (->> c
       (map #(get autocomplete-scores %))
       (reduce #(+ (* %1 5) %2) 0)))

(defn pick-middle [c]
  (let [mid-point (quot (count c) 2)]
    (get (into [] c) mid-point)))
    

(defn part2-fn [d]
  "Takes prep-data"
  (->> (map validate-line d)
       (filter #(not (contains? close-brackets %)))
       (map #(map-open-to-close %))
       (map #(score-autocomplete %))
       sort
       pick-middle))

(def part2 (part2-fn (prep-data data)))
   
(println (str "Answer for Part 2 is " part2))


  
