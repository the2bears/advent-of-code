(ns a-o-c-2021.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(comment "https://adventofcode.com/2021/day/14")

(def test-data
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def data
  (-> (slurp (io/resource "day14.txt"))))

(defn prep-data [d]
  (let [template (-> d
                     str/split-lines
                     (->> (take-while #(> (count %) 0))
                          first))
                          ;;(partition 2 1)))
        rules (-> d
                   str/split-lines
                   (->> (drop-while #(> (count %) 0))
                        (drop 1)
                        (map #(str/split % #" "))
                        (map #(vector (seq (first %)) (last %)))
                        (reduce #(assoc %1
                                        (first %2)
                                        (first (char-array (second %2))))
                                {})))]
    {:template template :rules rules}))

(comment
  (map #(vector (first %1) %2 (second %1)) (:template t) t2))

(defn polymer-step [pd]
  "Takes prep-data d, returns with :template updated to next iteration"
  (let [template (partition 2 1 (:template pd))
        via-rules (map #(get (:rules pd) %) template)
        applied-rules (map #(vector (first %1) %2 (second %1)) template via-rules)
        head-result (first applied-rules)
        tail-result (->> (rest applied-rules)
                         (map #(drop 1 %)))
        string-result (->> (cons head-result tail-result)
                           (apply concat)
                           (apply str))]                                  
      (assoc pd :template string-result)))


(defn n-steps [pd n]
  (reduce (fn [acc v]
             (polymer-step acc))
          pd
          (range n)))

(defn part1-fn [d n]
  (let [pd (n-steps (prep-data d) n)
        freq (frequencies (:template pd))
        values (vals freq)
        answer (- (apply max values) (apply min values))]
    answer))

(def part1 (part1-fn data 10))

(println (str "Answer for Part 1 is " part1))

;;;Part 2 is forty iterations, above is far too slow.
(def a (make-array Integer/TYPE 10))

(defn polymer-step-2 [pd]
  (let [template (:template pd)
        chars (into-array Character/TYPE (sequence template))
        l (alength chars)
        al (java.util.ArrayList.)
        rules (:rules pd)]
    (doseq [i (range 1 l)]
      (let [k (list (aget chars (- i 1)) (aget chars i))
            ch (get rules k)]
;;            _ (prn (str "ch " ch ", k " k))]
        (.add al (aget chars (- i 1)))
        (.add al ch)))
    (.add al (last template))
    (assoc pd :template (apply str (.toArray al)))))
  
(defn n-steps-2 [pd n]
  (reduce (fn [acc v]
            (polymer-step-2 acc))
          pd
          (range n)))

(defn part2-fn [d n]
  (let [pd (n-steps-2 (prep-data d) n)
        freq (frequencies (:template pd))
        values (vals freq)
        answer (- (apply max values) (apply min values))]
    answer))

(defn pair-frequencies [pd]
  (let [template (:template pd)
        pf  (->> template
                 (partition 2 1)
                 frequencies)]
    (assoc pd :pair-frequencies pf)))

(defn update-frequencies [rules freq]
  (reduce (fn [acc [[a b :as word] n]]
            (let [c (rules word)]
              (-> acc
                  (update (list a c) #(+ (or % 0) n))
                  (update (list c b) #(+ (or % 0) n)))))
          {}
          freq))

(defn polymer-step-fast [pd]
  (let [freq (:pair-frequencies pd)
        rules (:rules pd)]
    (assoc pd :pair-frequencies (update-frequencies rules freq))))


(defn solution-fn [d n]
  (let [pd (-> d
               prep-data
               pair-frequencies)
        final-pair-frequencies (reduce (fn [acc _]
                                         (polymer-step-fast acc))
                                       pd
                                       (range n))
        char-freq (reduce (fn[acc [[a b] n]]
                            (update acc a #(+ (or % 0) n)))
                          {(last (:template pd)) 1}
                          (:pair-frequencies final-pair-frequencies))
        sorted (sort-by - (vals char-freq))]
                          
    (->> sorted
         (apply (juxt max min))
         (apply -))))
                
   
(def fast-part1 (solution-fn data 10))
(println (str "Answer for fast Part 1 is " fast-part1))

(def fast-part2 (solution-fn data 40))
(println (str "Answer for fast Part 2 is " fast-part2))
