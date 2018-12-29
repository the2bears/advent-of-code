(ns a-o-c-2018.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day12-initial-state "##.#.#.##..#....######..#..#...#.#..#.#.#..###.#.#.#..#..###.##.#..#.##.##.#.####..##...##..#..##.#.")

(def day12-rules
  "...## => #
   #.#.# => #
   .###. => #
   #.#.. => .
   .#..# => #
   #..#. => #
   ..##. => .
   ....# => .
   #.... => .
   ###.. => #
   .#### => #
   ###.# => .
   #..## => #
   ..... => .
   ##.## => #
   ####. => .
   ##.#. => .
   #...# => .
   ##### => .
   ..#.. => .
   .#.#. => .
   #.### => .
   .##.# => .
   ..#.# => .
   .#.## => #
   ...#. => .
   ##... => #
   ##..# => #
   .##.. => .
   .#... => #
   #.##. => #
   ..### => .")

(def test-initial-state "#..#.#..##......###...###")

(def test-rules
  "...## => #
   ..#.. => #
   .#... => #
   .#.#. => #
   .#.## => #
   .##.. => #
   .#### => #
   #.#.# => #
   #.### => #
   ##.#. => #
   ##.## => #
   ###.. => #
   ###.# => #
   ####. => #")

(defn get-five [x state]
  (let [r (range (- x 2) (+ x 3))]
    (mapv #(get state % \.) r)))
  
(defn parse-rules [data]
  (->> data
       (str/split-lines)
       (filter #(= \# (last %)))
       (map str/trim)
       (map (partial drop-last 5))
       (map (partial into []))
       (into #{})))

(get (into [] test-initial-state) -1 \.)

(defn prep-state [state]
  {:state (into [] state)
   :offset 0})

(defn clear-leading-dots [{:keys [state offset]}]
  (let [cleared-state (into [] (drop-while #(= \. %) state))]
    {:state cleared-state
     :offset (+ offset (- (count state) (count cleared-state)))}))

(defn iterate-state [{:keys [state offset]} rules]
  (let [r (range -4 (+ (count state) 2))]
    {:state (mapv (fn[n]
                    (let [five (get-five n state)]
                      (if (rules five) \# \.)))
                  r)
     :offset (+ offset -4)}))

(defn sum-state [{:keys [state offset]}]
  (let [r (range 0 (count state))]
    (reduce (fn [acc c]
              (+ acc (if (= (get state c) \#) (+ offset c) 0)))
            0
            r)))

(defn day12-p1 [initial-state rules c]
  (let [i-s (prep-state initial-state)
        rules (parse-rules rules)]
    (loop [state i-s
           c c]
      (if (= c 0)
        (sum-state state)
        (recur (clear-leading-dots (iterate-state state rules))
               (dec c))))))
;;2140






