(ns a-o-c-2017.day25)
 

;left is -1, right is +1

(defn print-tape [tape]
  (let [min-pc (apply min (keys tape))
        max-pc (apply max (keys tape))]
    (doseq [x (range min-pc (inc max-pc))]
      (prn x))))

(defn state-a
  " If the current value is 0:
      - Write the value 1.
      - Move one slot to the right.
      - Continue with state B.
    If the current value is 1:
      - Write the value 0.
      - Move one slot to the left.
      - Continue with state C."
  [tape cursor vac]
  (if (= vac 0)
    (assoc {} :tape (assoc tape cursor 1)
           :cursor (inc cursor)
           :state \B)
    (assoc {} :tape (assoc tape cursor 0)
           :cursor (dec cursor)
           :state \C)))


(defn state-b
  " If the current value is 0:
      - Write the value 1.
      - Move one slot to the left.
      - Continue with state A.
    If the current value is 1:
      - Write the value 1.
      - Move one slot to the left.
      - Continue with state D."
  [tape cursor vac]
  (if (= vac 0)
    (assoc {} :tape (assoc tape cursor 1)
           :cursor (dec cursor)
           :state \A)
    (assoc {} :tape tape
           :cursor (dec cursor)
           :state \D)))

(defn state-c
  " If the current value is 0:
      - Write the value 1.
      - Move one slot to the right.
      - Continue with state D.
    If the current value is 1:
      - Write the value 0.
      - Move one slot to the right.
      - Continue with state C."
  [tape cursor vac]
  (if (= vac 0)
    (assoc {} :tape (assoc tape cursor 1)
           :cursor (inc cursor)
           :state \D)
    (assoc {} :tape (assoc tape cursor 0)
           :cursor (inc cursor)
           :state \C)))

(defn state-d
  " If the current value is 0:
      - Write the value 0.
      - Move one slot to the left.
      - Continue with state B.
    If the current value is 1:
      - Write the value 0.
      - Move one slot to the right.
      - Continue with state E."
  [tape cursor vac]
  (if (= vac 0)
    (assoc {} :tape tape
           :cursor (dec cursor)
           :state \B)
    (assoc {} :tape (assoc tape cursor 0)
           :cursor (inc cursor)
           :state \E)))

(defn state-e
  " If the current value is 0:
      - Write the value 1.
      - Move one slot to the right.
      - Continue with state C.
    If the current value is 1:
      - Write the value 1.
      - Move one slot to the left.
      - Continue with state F."
  [tape cursor vac]
  (if (= vac 0)
    (assoc {} :tape (assoc tape cursor 1)
           :cursor (inc cursor)
           :state \C)
    (assoc {} :tape tape
           :cursor (dec cursor)
           :state \F)))

(defn state-f
  " If the current value is 0:
      - Write the value 1.
      - Move one slot to the left.
      - Continue with state E.
    If the current value is 1:
      - Write the value 1.
      - Move one slot to the right.
      - Continue with state A."
  [tape cursor vac]
  (if (= vac 0)
    (assoc {} :tape (assoc tape cursor 1)
           :cursor (dec cursor)
           :state \E)
    (assoc {} :tape tape
           :cursor (inc cursor)
           :state \A)))

(def steps 12656374)

                                        ;2526 is the correct answer
(defn part-1 [turns]
  (let [start-state \A
        cursor 0
        tape {0 0}]
    ;(print-tape tape)
    (loop [tape tape
           cursor cursor
           state start-state
           c 0]
      ;(prn :tape (count tape));(print-tape tape)
      (if (= c turns)
        (let [v-tape (into [] tape)
              ones (filter (fn[[x y]] (= y 1)) v-tape)]
          (count ones))
        (let [value-at-cursor (get tape cursor 0)
              {:keys[tape cursor state] :as next-step}
              (case state
                \A (state-a tape cursor value-at-cursor)
                \B (state-b tape cursor value-at-cursor)
                \C (state-c tape cursor value-at-cursor)
                \D (state-d tape cursor value-at-cursor)
                \E (state-e tape cursor value-at-cursor)
                \F (state-f tape cursor value-at-cursor))]
          (recur tape cursor state (inc c)))))))    
                                                              
(defn part-2 [])


  
