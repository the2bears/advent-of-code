(ns a-o-c-2018.day9)

(split-at 1 [0])

(split-at 1 [0 1])

(defn next-vector [^long current-pos ^long next-value v]
  (let [special-case? (= 0 (mod next-value 23))
        next-index (if special-case?
                     (- current-pos 7)
                     (+ 1 (mod (+ current-pos 1) (count v))))
        next-index (if (< next-index 0)
                     (+ next-index (count v))
                     next-index)
        split-at (+ next-index (if special-case? 1 0))
        [left right] [(subvec v 0 split-at) (subvec v split-at (count v))]
        score (if special-case?
                (+ next-value (last left))
                0)]
    [next-index
     score
     (->> (if special-case?
            (into (subvec left 0 (dec (count left))) right)
            (into (conj left next-value) right))
          (into []))]))

(next-vector 3 4 [0 2 1 3])

(next-vector 13 23 [0 16  8 17  4 18  9 19  2 20 10 21  5 22 11  1 12  6 13  3 14  7 15])

(next-vector 6 24 [0 16  8 17  4 18 19 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15]) 

(defn day9-p1 [num-players num-marbles]
  (let [players (cycle (range 1 (inc num-players)))]
    (loop [players players
           v [0]
           scores {}
           marbles (range 1 (inc num-marbles))
           current-pos 1]
      (if (seq marbles)
        (let [[next-index score next-v] (next-vector current-pos
                                                     (first marbles)
                                                     v)
              next-scores (update scores (first players) (fnil + 0) score)]
          (recur (rest players)
                 next-v
                 next-scores
                 (rest marbles)
                 next-index))
        (->> scores
             (into [])
             (sort-by second)
             ((comp last last)))))))      

;;10 players; last marble is worth 1618 points: high score is 8317
;;13 players; last marble is worth 7999 points: high score is 146373
;;17 players; last marble is worth 1104 points: high score is 2764
;;21 players; last marble is worth 6111 points: high score is 54718
;;30 players; last marble is worth 5807 points: high score is 37305

(time (day9-p1 30 5807))
;;(time (day9-p1 410 72059))
;;429287

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true))
