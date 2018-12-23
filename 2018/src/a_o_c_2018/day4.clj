(ns a-o-c-2018.day4
  (:require [clojure.java.io :as io]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))

;;;[1518-10-10 23:52] Guard #2707 begins shift
;;;[1518-07-19 00:42] wakes up
;;;[1518-06-16 00:41] falls asleep

(def day4-data
  (->> (io/reader "resources/day4.txt")
       (line-seq)))

(def pound (sym \#))
(def guard (<*> (>> pound dec-num)))

(def open-sq (sym \[))
(def close-sq (sym \]))
(def dash (sym \-))
(def y-m-d (<*> dec-num (>> dash dec-num) (>> dash dec-num)))
(def time* (<*> dec-num (>> colon dec-num)))
(def date-time (between open-sq close-sq (<*> y-m-d (skip-ws time*))))

(def command (token* "Guard" "wakes" "falls"))
(def command-line (<*> command (skip-ws (<<(<|> guard split) (optional split)))))
(def record (<*> date-time (skip-ws command-line)))

(value guard "#142")
(value date-time "[1518-10-10 23:52]")

(def r1 (value record "[1518-10-10 23:52] Guard #2707 begins shift"))
(def r2 (value record "[1518-07-19 00:42] wakes up"))
(value command-line "Guard #2707 begins shift")
(value command-line "wakes up")

(defn record-sort [[y1 mon1 d1 h1 min1 _ _]
                   [y2 mon2 d2 h2 min2 _ _]]
                   
  (let [c (compare y1 y2)]
    (if-not (= c 0)
      c
      (let [c (compare mon1 mon2)]
        (if-not (= c 0)
          c
          (let [c (compare d1 d2)]
            (if-not (= c 0)
              c
              (let [c (compare h1 h2)]
                (if-not (= c 0)
                  c
                  (compare min1 min2))))))))))


(defn get-guard [current-guard [y M d h m cmd id]]
  (if (= "Guard" cmd)
    id
    current-guard))

(defn get-min [current-min [y M d h m cmd id]]
  (if (#{"falls" "wakes"}  cmd)
    m
    current-min))

(defn get-cmd [[y M d h m cmd id]]
  cmd)

(defn get-day-hour-min [[y M d h m cmd id]]
  [d h m])

(defn update-guard-day [v cmd from to]
  (let [the-into (if (seq v) v [])
        the-char (if (= "wakes" cmd) \# \.)]
    (into the-into (repeat (- to from) the-char))))  

(defn process-data [data]
  (->> data
       (map (partial value record) data)
       (map flatten)
       (sort #(record-sort %1 %2))))

(defn pad-minutes [v]
  (let [pad-char (if (= \# (last v)) \. \#)]
    (into v (repeat (- 60 (count v)) pad-char))))

(defn key-vectors [schedule]
  (apply concat (for [guard (keys schedule)]
                  (for [day (keys (get schedule guard))]
                    [guard day]))))

(defn pad-schedule [schedule]
  (let [key-combos (key-vectors schedule)]
    (reduce #(update-in %1 %2 pad-minutes) schedule key-combos)))

(defn minutes-asleep [schedule guard]
  (let [guard-days (get schedule guard)
        hours (vals guard-days)]
    (->> hours
         (map (partial filter #(= \# %)))
         (map count)
         (apply +))))

(defn sleepiest-minute [schedule guard]
  (let [guard-days (get schedule guard)
        hours (vals guard-days)]
    (->> hours
         (map (partial map #(if (= \# %) 1 0)))
         (apply (partial map +))
         (map-indexed vector)
         (sort-by second)
         (last)
         (first))))

(defn sleepiest-indexed-minute [schedule guard]
  (let [guard-days (get schedule guard)
        hours (vals guard-days)]
    (->> hours
         (map (partial map #(if (= \# %) 1 0)))
         (apply (partial map +))
         (map-indexed vector)
         (sort-by second)
         (last)
         (flatten))))
      
(defn build-schedule [data]
  (loop [records (process-data data)
         current-guard 0
         current-min 0
         acc {}]
    (if-not (seq records)
      acc
      (let [r (first records)
            current-guard' (get-guard current-guard r)
            cmd (get-cmd r)
            [day hour m] (get-day-hour-min r)
            current-min' (if (= "Guard" cmd)
                           0
                           (get-min current-min r))]
            ;;_ (prn :cmd cmd :day day :last-min current-min :current-min current-min' :current-guard current-guard')]
        (cond
          (= 0 current-guard')
          (recur (rest records) current-guard' current-min' acc)
          (= "Guard" cmd)
          (recur (rest records) current-guard' current-min' acc)
          :else
          (recur (rest records)
                 current-guard'
                 current-min'
                 (update-in acc [current-guard' day] update-guard-day cmd current-min current-min')))))))

(defn day4-p1 [data]
  (let [padded-schedule (->> data
                             (build-schedule)
                             (pad-schedule))
        minutes-slept (map #(vector % (minutes-asleep padded-schedule %)) (keys padded-schedule))
        laziest-guard (->>  minutes-slept
                            (sort-by second)
                            last
                            first)
        sleep-minutes (sleepiest-minute padded-schedule laziest-guard)]
    (* laziest-guard sleep-minutes)))

(day4-p1 day4-data)
;;77084

(defn day4-p2 [data]
  (let [padded-schedule (->> data
                             (build-schedule)
                             (pad-schedule))
        sleepiest-minutes (->>  (keys padded-schedule)
                                (map #(concat (vector %) (sleepiest-indexed-minute padded-schedule %)))
                                (sort-by last)
                                (last))]
    (* (first sleepiest-minutes) (second sleepiest-minutes))))                                        

(day4-p2 day4-data)
;;23047
