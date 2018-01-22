(ns a-o-c-2017.day18
  (:require [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]
            [clojure.core :refer :all :rename {mod c-mod}]
            [clojure.string :as str]))

(def data "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 622
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19
  ")

(def cmd (skip-ws (<+> letter letter letter)))
(def register (skip-ws (<|> letter dec-lit)))
(def n (skip-ws (<|> letter dec-lit)))
(def cmd-line (<*> cmd register (optional n)))

(defn num-or-reg [arg registers]
  (if (number? arg) arg (get registers arg 0)))

(defn set [[x y] {:keys [pc registers] :as context}]
  (let [registers (assoc registers x (num-or-reg y registers))]
    (assoc context
           :pc (inc pc)
           :registers registers)))

(defn mul [[x y] {:keys [pc registers] :as context}]
  (let [registers (assoc registers x (* (num-or-reg x registers)
                                        (num-or-reg y registers)))]
    (assoc context
           :pc (inc pc)
           :registers registers)))

(defn add [[x y] {:keys [pc registers] :as context}]
  (let [registers (assoc registers x (+ (num-or-reg x registers)
                                        (num-or-reg y registers)))]
    (assoc context
           :pc (inc pc)
           :registers registers)))

(defn jgz [[x y] {:keys [pc registers] :as context}]
  (let [x (num-or-reg x registers)
        y (num-or-reg y registers)]
    (if (> x 0)
      (assoc context
             :pc (+ pc y))
      (assoc context
             :pc (inc pc)))))
             
(defn snd [[hz _] {:keys [pc registers] :as context}]
  (let [hz (num-or-reg hz registers)]
    (assoc context
           :hz hz
           :pc (inc pc))))

(defn mod [[x y] {:keys [pc registers] :as context}]
  (let [registers (assoc registers x (c-mod (num-or-reg x registers)
                                            (num-or-reg y registers)))]
    (assoc context
           :pc (inc pc)
           :registers registers)))

(defn rcv [[x _] {:keys [pc registers] :as context}]
  (let [x (num-or-reg x registers)]
    (if (= 0 x)
      (assoc context
             :pc (inc pc))
      (assoc context
             :pc (inc pc)
             :recovered (:hz context)))))

(defn dispatch [[cmd & args] context]
;  (prn :dispatch cmd args :pc (:pc context))
  (case cmd
    "set" (set args context)
    "add" (add args context)
    "mul" (mul args context)
    "mod" (mod args context)
    "snd" (snd args context)
    "rcv" (rcv args context)
    "jgz" (jgz args context)))           

(defn parse-lines [data]
  (->> data
       (str/split-lines)
       (map #(value cmd-line %))
       (into [])))

(def test-data "set a 1
add a 2/
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")


                                        ;9423 is the correct answer
(defn part-1 [data]
  (let [ins-set (parse-lines data)
        context {:registers {} :pc 0}]
    (loop [context context]
      (if (:recovered context)
        (:hz context)
        (let [pc (:pc context)]
          (recur (dispatch (get ins-set pc) context)))))))

(defn snd2 [[x _] {:keys [pc out sent registers shared-memory] :or {sent 0} :as context}]
  (let [v (num-or-reg x registers)
        q (get shared-memory out '())
        updated-sm (assoc shared-memory out (cons v q))]
    (assoc context
           :pc (inc pc)
           :sent (inc sent)
           :shared-memory updated-sm)))

(snd2 '(\x 5)
      {:registers {\x 6} :pc 0 :out :2 :sent 2 :shared-memory {:2 '(4 3 4)}})

(defn rcv2 [[x _] {:keys [pc registers in shared-memory] :as context}]
  (let [q (get shared-memory in)]
    (if (seq q)
      (assoc context
             :wait false
             :registers (assoc registers x (last q))
             :pc (inc pc)
             :shared-memory (assoc shared-memory in (drop-last q)))
      (assoc context
             :wait true))))

(rcv2 '(\x 5)
      {:registers {\x 6} :pc 0 :in :1 :out :2 :shared-memory {:1 '(5 6) :2 '(4 3 4)}})


(defn dispatch2 [[cmd & args] context]
  ;(prn :dispatch2 cmd args :pc (:pc context))
  (case cmd
    "set" (set args context)
    "add" (add args context)
    "mul" (mul args context)
    "mod" (mod args context)
    "snd" (snd2 args context)
    "rcv" (rcv2 args context)
    "jgz" (jgz args context)))           


                                        ;7620 is the correct answer
(defn part-2 [data]
  (let [ins-set (parse-lines data)
        cp1 {:registers {\p 0} :pc 0 :in :0 :out :1 :wait false}
        cp2 {:registers {\p 1} :pc 0 :in :1 :out :0 :wait false}
        shared-memory {:0 '() :1 '()}]
    (loop [cp1 cp1
           cp2 cp2
           shared-memory shared-memory]
      (if (and (:wait cp1)
               (:wait cp2))
        (prn :deadlock :cp1 (:sent cp1) :cp2 (:sent cp2))
        (let [{:keys [shared-memory]
               :as cp1} (dispatch2 (get ins-set (:pc cp1))
                                   (assoc cp1 :shared-memory shared-memory))  
              {:keys [shared-memory]
               :as cp2} (dispatch2 (get ins-set (:pc cp2))
                                   (assoc cp2 :shared-memory shared-memory))]  
          (recur cp1 cp2 shared-memory))))))
              












