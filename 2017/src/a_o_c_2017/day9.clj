(ns a-o-c-2017.day9
  (:require [clojure.java.io :as io]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))
(def data
  (slurp (io/resource "day9.txt")))
  
                                        ;8337 is the correct answer
(defn part-1 [data]
  (loop [data data
         state {}
         level 0
         total 0]
    (if (seq data)
      (let [c (first data)
            the-rest (rest data)]
        (case c
          \! (recur (rest the-rest) state level total)
          \< (recur the-rest (assoc state :garbage-in true) level total)
          \> (recur the-rest (assoc state :garbage-in false) level total)
          \{ (if (:garbage-in state)
               (recur the-rest state level total)
               (recur the-rest state (inc level) total))
          \} (if (:garbage-in state)
               (recur the-rest state level total)
               (recur the-rest state (dec level) (+ total level)))
          (recur the-rest state level total)))
      total)))

                                        ;4330 is the correct answer
(defn part-2 [data]
  (loop [data data
         state {}
         cancelled 0]
    (if (seq data)
      (let [c (first data)
            the-rest (rest data)]
        (case c
          \! (recur (rest the-rest) state cancelled)
          \< (if (:garbage-in state)
               (recur the-rest state (inc cancelled))
               (recur the-rest (assoc state :garbage-in true) cancelled))
          \> (recur the-rest (assoc state :garbage-in false) cancelled)
          (recur the-rest state (if (:garbage-in state)
                                  (inc cancelled)
                                  cancelled))))
      cancelled)))


