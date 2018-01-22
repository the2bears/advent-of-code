(ns a-o-c-2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
            
(def data
  (-> (slurp (io/resource "day13.txt"))
      (str/split-lines)
      (->> (map #(str/split % #":")))
      (->> (reduce #(assoc %1 (Integer/parseInt (str/trim (first %2)))
                           (Integer/parseInt (str/trim (second %2))))
                   {}))))

(def data2
  (-> "0: 3
1: 2
4: 4
6: 4"
      (str/split-lines)
      (->> (map #(str/split % #":")))
      (->> (reduce #(assoc %1 (Integer/parseInt (str/trim (first %2)))
                           (Integer/parseInt (str/trim (second %2))))
                   {}))))

(defn- scanner-layer [picos level layer-range]
  (let [cycle-len (* 2 (dec layer-range))
        scan-pos (mod picos cycle-len)
        _ (prn :picos picos :level level :scan-pos scan-pos :cycle-len cycle-len)]
    (if (= 0 scan-pos)
      (* level layer-range)
      0)))


                                        ;1476 is the correct answer
(defn part-1* [picos data]
  (let [max-k (apply max (keys data))]
    (loop [picos picos
           level 0
           danger (scanner-layer picos level (get data 0))]
      (if (> level max-k)
        danger
        (let [new-picos (inc picos)
              new-level (inc level)
              layer-range (get data new-level)]
          (recur new-picos
                 new-level
                 (if layer-range
                   (+ danger (scanner-layer new-picos new-level layer-range))
                   danger)))))))

(defn part-1 [data]
  (part-1* 0 data))

(defn- danger? [picos level layer-range]
  (let [cycle-len (* 2 (dec layer-range))
        scan-pos (mod picos cycle-len)]
    (= 0 scan-pos)))

(defn part-2* [picos data]
  (let [max-k (apply max (keys data))]
    (loop [picos picos
           level 0
           danger (danger? picos level (get data 0))]
      (if (or danger
              (> level max-k))
        danger
        (let [new-picos (inc picos)
              new-level (inc level)
              layer-range (get data new-level)
              new-danger (or danger (if layer-range
                                      (danger? new-picos new-level layer-range)
                                      false))]
          (recur new-picos
                 new-level
                 new-danger))))))

                                        ;3937334 is the correct answer
(defn part-2 [data]
  (loop [delay 0
         danger (part-2* 0 data)]
    (if-not danger
      delay
      (recur (inc delay)
             (part-2* (inc delay) data)))))
