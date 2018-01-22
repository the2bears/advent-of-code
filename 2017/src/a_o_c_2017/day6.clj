(ns a-o-c-2017.day6)

(def data [0  5  10  0  11  14  13  4  11  8  8  7  1  4  12  11])

(defn first-max [v]
  (let [mx (apply max v)]
    (loop [i 0]
      (if (= mx (v i))
        i
        (recur (inc i))))))
      
(defn dist [v i]
  (let [n (v i)]
    (loop [v (assoc v i 0)
           i (if (= i (dec (count v))) 0 (inc i))
           n n]
      (if (= 0 n)
        v
        (let [n-i (if (= i (dec (count v)))
                    0
                    (inc i))]
          (recur (assoc v i (inc (v i))) n-i (dec n)))))))          

                                        ;7864 is the correct answer

(defn part-1 [v]
  (loop [v v
         step 0
         seen #{}]
    (if (seen v)
      step
      (let [n-v (dist v (first-max v))
            n-seen (conj seen [v step])
            n-step (inc step)]
        (recur n-v n-step n-seen)))))

                                        ;1695 is the correct answer
(defn part-2 [v]
  (loop [v v
         step 0
         seen #{}
         seen-step #{}]
    (if (seen v)
      (let [f-seen (first (filter #(= v (first %)) seen-step))]
        (- step (second f-seen)))
      (let [n-v (dist v (first-max v))
            n-seen (conj seen v)
            n-seen-step (conj seen-step [v step])
            n-step (inc step)]
        (recur n-v n-step n-seen n-seen-step)))))

(part-2 data)
