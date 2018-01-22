(ns a-o-c-2017.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [blancas.kern.core :refer :all]
            [blancas.kern.lexer.basic :refer :all]))

(def data
  (slurp (io/resource "day20.txt")))

(defn- manhattan-distance [[x y z]]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(def three-tuple (angles (sep-by (sym \,) dec-lit)))

(def assignment (<$> (fn[[a b c]]{(keyword (str a)) c
                                  (str "d" a) (manhattan-distance c)})
                     (<*> letter (sym \=) three-tuple)))

(def particle-line (<$> #(apply merge %) (sep-by (sym \,) assignment)))

(defn process-data [data]
  (-> data
      (str/split-lines)
      (->> (map #(value particle-line %))
           (map-indexed vector)
           (sort-by #((second %) "da")))))
           ;(into {})))) 

(comment
  "91 is the correct answer for part 1, only particle with da of 0")

(defn tick-tuple [[id {:keys [p v a] :as m}]]
  (let [v (map + v a)
        p (map + p v)]
    [id (assoc m
               :p p
               :v v
               "dp" (manhattan-distance p)
               "dv" (manhattan-distance v))]))

(defn extract-pos [[id {:keys [p v a] :as m}]]
  p)


(comment
  "567 is the correct answer, after clearing collided particles < 100 iterations")

(defn part-2 [data]
  (let [data (process-data data)
        test (first data)
        _ (prn test)]
    (loop [data data
           c 0
           grt-1 0]
      (if (> c 100)
        [c grt-1]
        (let [data (map tick-tuple data)
              all-points (map extract-pos data)
              ap-freq (frequencies all-points)
              grt-1 (filter #(> (second %) 1) ap-freq)
              removals (if (seq grt-1)
                         (set (map first grt-1))
                         #{})
              data (remove #(removals (:p (second %))) data)
              _ (prn :count (count data) :removals removals)]
          (recur data
                 (inc c)
                 grt-1))))))    
    
