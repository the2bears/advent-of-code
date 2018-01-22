(ns a-o-c-2017.day15
  (:require [clojure.string :as str]))

                                        ;Generator A starts with 116

                                        ;Generator B starts with 299

(def a-factor (biginteger 16807))

(def b-factor (biginteger 48271))

(def divisor (biginteger 2147483647))

(defn to-bin [i]
  (let [hex-str (Integer/toBinaryString i)
        padding (- 32 (count hex-str))
        padding-str (apply str (take padding (repeatedly (fn[]"0"))))]    
    (str padding-str hex-str)))

(defn day15-seq [seed fctr divisor]
  ((fn generate [a] 
     (lazy-seq (cons a (generate (mod (* (biginteger a) fctr) divisor)))))
   (mod (* (biginteger seed) fctr) divisor)))

(def a-generator (day15-seq 65 a-factor divisor))

(def b-generator (day15-seq 8921 b-factor divisor))

(apply str (drop 16 (to-bin (first a-generator))))

(take 5 a-generator)

(take 5 b-generator)


                                        ;569 is the correct answer
(defn part-1 [n a-fct b-fct]
  (loop [a-gen (day15-seq a-fct a-factor divisor)
         b-gen (day15-seq b-fct b-factor divisor)
         c n
         eq 0]
    (if (= c 0)
      eq
      (let [next-a (drop 16 (to-bin (first a-gen)))
            next-b (drop 16 (to-bin (first b-gen)))]
        (when (= 0 (mod c 100000))
          (prn :c c))
        (recur (rest a-gen)
               (rest b-gen)
               (dec c)
               (if (= next-a next-b)
                 (inc eq)
                 eq))))))


(defn part-2 [n a-fct b-fct]
  (loop [a-gen (filter #(= 0 (mod % 4)) (day15-seq a-fct a-factor divisor))
         b-gen (filter #(= 0 (mod % 8)) (day15-seq b-fct b-factor divisor))
         c n
         eq 0]
    (if (= c 0)
      eq
      (let [next-a (drop 16 (to-bin (first a-gen)))
            next-b (drop 16 (to-bin (first b-gen)))]
        (when (= 0 (mod c 100000))
          (prn :c c))
        (recur (rest a-gen)
               (rest b-gen)
               (dec c)
               (if (= next-a next-b)
                 (inc eq)
                 eq))))))

