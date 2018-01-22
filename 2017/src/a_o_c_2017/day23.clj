(ns a-o-c-2017.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))


(def data
  (slurp (io/resource "day23.txt")))

(def cmd (skip-ws (<+> letter letter letter)))
(def register (skip-ws (<|> letter dec-lit)))
(def cmd-line (<*> cmd register register))

(defn num-or-reg [arg registers]
  (if (number? arg) arg (get registers arg 0)))

(defn process-data [data]
  (->> data
       (str/split-lines)
       (map #(value cmd-line %))
       (into [])))

(defn set [[x y] {:keys [pc registers] :as context}]
  (let [registers (assoc registers x (num-or-reg y registers))]
    (assoc context
           :pc (inc pc)
           :registers registers)))

(defn mul [[x y] {:keys [pc registers mul-invoke] :or {mul-invoke 0} :as context}]
  (let [registers (assoc registers x (* (num-or-reg x registers)
                                        (num-or-reg y registers)))]
    (assoc context
           :pc (inc pc)
           :registers registers
           :mul-invoke (inc mul-invoke))))

(defn sub [[x y] {:keys [pc registers] :as context}]
  (let [registers (assoc registers x (- (num-or-reg x registers)
                                        (num-or-reg y registers)))]
    (assoc context
           :pc (inc pc)
           :registers registers)))

(defn jnz [[x y] {:keys [pc registers] :as context}]
  (let [x (num-or-reg x registers)
        y (num-or-reg y registers)]
    (if (not= x 0)
      (assoc context
             :pc (+ pc y))
      (assoc context
             :pc (inc pc)))))

(defn dispatch [[cmd & args] context]
  (case cmd
    "set" (set args context)
    "sub" (sub args context)
    "mul" (mul args context)
    "jnz" (jnz args context)))           


                                        ;8281 is the correct answer
(defn part-1 [data]
  (let [ins-set (process-data data)
        _ (prn :ins-set (count ins-set))
        min-pc 0
        max-pc (dec (count ins-set))]
    (loop [pc 0
           context {:registers {} :pc 0}]
      (if (<= min-pc pc max-pc)
        (let [ins (get ins-set pc)
              new-context (dispatch ins context)]
          (recur (:pc new-context) new-context))
        context))))

(defn any? [l]
  (reduce #(or %1 %2) l))

(defn prime? [n]
  (if (even? n)
    false
    (let [root (num (int (Math/sqrt n)))]
      (loop [i 3]
        (if (> i root)
          true
          (if (zero? (mod n i)) false
            (recur (+ i 2))))))))

                                        ;911 is the correct answer
(defn part-2
  "The test program exits when b is finally equal to c.
   This is the range of 109300 to 126300. b is increased each
   loop by 17. This taks a *long* time. h is increased each time
   17 is added to be, with the exception of prime values of b.
   In this case f is never set to 0, the h increase is skipped."
  []
  (let [all-nums (range 109300 (inc 126300) 17)
        primes (filter prime? all-nums)]
    (- (count all-nums) (count primes))))
