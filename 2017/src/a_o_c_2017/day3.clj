(ns a-o-c-2017.day3)

(def data 289326)


(def spiral-seq 
  ((fn spiral [[a b c]] 
     (lazy-seq (cons [a b c] (spiral [(+ 2 a)
                                      (+ 4 (* a 4))
                                      (+ c (+ 4 (* a 4)))]))))
   [1 1 1]))


(defn cardinal [[shell-num shell-count acc]]
  (let [left (+ (- acc shell-count) (quot shell-num 2))
        cards (for [x (range 4)]
                (+ left (* x (dec shell-num))))]
    cards))


(defn to-nearest [n cardinals]
  (let [diffs (->> cardinals
                   (map #(- % n))
                   (map #(Math/abs %))
                   (apply min))]
    diffs))


(defn- which-shell [n]
  (loop [shell-spiral spiral-seq]
    (let [[a b c] (first shell-spiral)]
      (if (<= n c)
        [a b c]
        (recur (rest shell-spiral))))))


                                        ;49 is the correct answer
(defn part-1 [n]
  (if (= 1 n)
    0
    (let [shell (which-shell n)
          [a b c] shell
          cost (+ (to-nearest n (cardinal shell)) (quot a 2))]
      cost)))

                                        ;=================


(defn neighbours [p]
  (for [dx [-1 0 1] dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ dx (first p)) (+ dy (second p))]))

(defn add-nbrs-vals [points nbrs acc-map]
  (let [];_ (prn :points points)
        ;_ (prn :nbrs nbrs)
        ;_ (prn :acc-map acc-map)]
    (reduce (fn [acc p]
              (let [];_ (prn :p p)]
                (if (points p)
                    (+ acc (acc-map p))
                    acc)))
            0
            nbrs)))

(defn- check-exceeds [state acc-map]
  (if-not (:found state)
    (let [potentials (vals (:acc state))
          exceeds (filter #(< (:target state) %) potentials)]
      (if (first exceeds)
        (assoc state :found (first exceeds))
        state))
    state))

(defn- up [state n]
  (let [{:keys [p-seq]} state
        sp (last p-seq)
        nx (inc (first sp))]
    (loop [{:keys [points p-seq acc] :as state} state
           steps 0]
      (if (= n steps)
        state
        (let [new-p [nx (+ steps (second sp))]
              nbrs (neighbours new-p)
              new-acc (assoc acc new-p (add-nbrs-vals points nbrs acc))
              new-p-seq (conj p-seq new-p)
              new-points (conj points new-p)
              new-state (check-exceeds state new-acc)]
          (recur (assoc new-state :p-seq new-p-seq
                        :points new-points
                        :acc new-acc)
                 (inc steps)))))))

(defn- left [state n]
  (let [{:keys [p-seq]} state
        sp (last p-seq)
        nx (dec (first sp))]
    (loop [{:keys [points p-seq acc] :as state} state
           steps 0]
      (if (= n steps)
        state
        (let [new-p [(- nx steps) (second sp)]
              nbrs (neighbours new-p)
              new-acc (assoc acc new-p (add-nbrs-vals points nbrs acc))
              new-p-seq (conj p-seq new-p)
              new-points (conj points new-p)
              new-state (check-exceeds state new-acc)]
          (recur (assoc new-state :p-seq new-p-seq
                        :points new-points
                        :acc new-acc)
                 (inc steps)))))))

(defn- down [state n]
  (let [{:keys [p-seq]} state
        sp (last p-seq)
        ny (dec (second sp))]
    (loop [{:keys [points p-seq acc] :as state} state
           steps 0]
      (if (= n steps)
        state
        (let [new-p [(first sp) (- ny steps)]
              nbrs (neighbours new-p)
              new-acc (assoc acc new-p (add-nbrs-vals points nbrs acc))
              new-p-seq (conj p-seq new-p)
              new-points (conj points new-p)
              new-state (check-exceeds state new-acc)]
          (recur (assoc new-state :p-seq new-p-seq
                        :points new-points
                        :acc new-acc)
                 (inc steps)))))))

(defn- right [state n]
  (let [{:keys [p-seq]} state
        sp (last p-seq)
        nx (inc (first sp))]
    (loop [{:keys [points p-seq acc] :as state} state
           steps 0]
      (if (= n steps)
        state
        (let [new-p [(+ nx steps) (second sp)]
              nbrs (neighbours new-p)
              new-acc (assoc acc new-p (add-nbrs-vals points nbrs acc))
              new-p-seq (conj p-seq new-p)
              new-points (conj points new-p)
              new-state (check-exceeds state new-acc)]
          (recur (assoc new-state :p-seq new-p-seq
                        :points new-points
                        :acc new-acc)
                 (inc steps)))))))

(def state {:points #{[0 0]}
            :p-seq [[0 0]]
            :acc {[0 0] 1}
            :target data})

                                        ;295229 is the correct answer

(defn part-2 [n]
  (let [state (assoc state :target n)
        side 2]
    (loop [state state
           side 2]
      (if (:found state)
        (:found state)
        (recur (-> state
                   (up side)
                   (left side)
                   (down side)
                   (right side))
               (+ 2 side))))))              


