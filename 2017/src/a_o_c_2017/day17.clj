(ns a-o-c-2017.day17)

(def data 376)

(def max-insert 2017)
(def max-2 50000000)
(def init-state [0])

(defn where-next [curr-pos v-len step-size]
  (mod (+ curr-pos step-size) v-len))

(defn insert-at [v pos n]
  (into [] (concat (conj (subvec v 0 (inc pos)) n) (subvec v (inc pos)))))


                                        ;777 is the correct answer
(defn part-1 [step-size]
  (let [spinlock (loop [spinlock init-state
                        curr-pos 0
                        n 0]
                   (if (> n max-insert)
                     spinlock
                     (let [w-n (where-next curr-pos
                                           (count spinlock)
                                           step-size)
                           next-sl (insert-at spinlock w-n (inc n))]
                       (recur next-sl (inc w-n) (inc n)))))]
    (loop [n 0]
      (if (= max-insert (get spinlock n))
        (get spinlock (mod (inc n) max-insert))
        (recur (inc n))))))


                                        ;39289581 is the correct answer
(defn part-2
  "Just run this, last one inserted at 0"
  [step-size]
  (let [spinlock (loop [curr-pos 0
                        n 1]
                   (if (> n max-2)
                     n
                     (let [w-n (where-next curr-pos
                                           n
                                           step-size)]
                       (when (= 0 w-n)
                         (prn :n n))
                       (recur (inc w-n) (inc n)))))]))
