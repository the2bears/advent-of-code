(ns a-o-c-2017.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def programs (into [] "abcdefghijklmnop"))
(def test-pgms (into [] "abcde"))

(set! *unchecked-math* true)

(defn read-data [f]
  (-> (slurp (io/resource f))
      (str/split-lines)
      (first)
      (str/split #",")))


(def test-data '("s1","x3/4","pe/b"))

(defn- parse-s [s]
  (let [n (Integer/parseInt (apply str s))]
    [\s n]))

(defn- parse-x [s]
  (let [s (str/split (apply str s) #"\/")
        from (Integer/parseInt (str (first s)))
        to (Integer/parseInt (str (last s)))]
    [\x from to])) 

(defn- parse-p [s]
  (let [a (first s)
        b (last s)]
    [\p a b]))

(defn- parse-cmd [s]
  (let [cmd (first s)]
    (case cmd
      \s (parse-s (rest s))
      \x (parse-x (rest s))
      \p (parse-p (rest s)))))

(defn- pre-parse-data [data]
  (map #(parse-cmd %) data))

(def data (pre-parse-data (read-data "day16.txt")))


(defn- spin [prgms s]
  (let [n (first s)
        not-n (- (count prgms) n)]
    (into [] (concat (subvec prgms not-n (count prgms)) (subvec prgms 0 not-n)))))

(defn- exchange [prgms s]
  (let [from (first s)
        to (last s)
        a (get prgms from)
        b (get prgms to)]
    (assoc prgms from b to a)))

(defn- rev-map [prgms]
  (reduce #(assoc %1 (second %2) (first %2)) {} (map-indexed vector prgms)))  

(defn- index-find [prgrms c]
  (loop [i 0]
    (let [a (get prgrms i)]
      (if (= c a)
        i
        (recur (inc i))))))
    
(defn- partner [prgms s]
  (let [a (first s)
        b (last s)
        from (index-find prgms a);
        to (index-find prgms b)];
    (assoc prgms from b to a)))

(defn- interpret [prgms s]
  (let [cmd (first s)]
    (case cmd
      \s (spin prgms (rest s))
      \x (exchange prgms (rest s))  
      \p (partner prgms (rest s)))))

                                        ;giadhmkpcnbfjelo is the correct answer
(defn part-1 [prgms script]
  (reduce interpret prgms script))
      ;(apply str)))

(defn cycle-size [prgrms script]
  (loop [acc prgrms
         c 0
         s #{}
         break false]
    (if break
      (count s)
      (let [new-s (conj s (apply str acc))]
        (recur (part-1 acc script)
               (inc c)
               new-s
               (= (count s) (count new-s)))))))


                                        ;njfgilbkcoemhpad is the correct answer
(defn part-2 [prgrms script n]
  (let [cycle-size (cycle-size prgrms script)
        cycle-mod (mod n cycle-size)]
    (loop [acc prgrms
           c 0
           s []]
      (if (= cycle-mod c)
        (apply str acc)
        (recur (part-1 acc script)
               (inc c)
               (conj s (apply str acc)))))))
