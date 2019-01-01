(ns a-o-c-2018.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def day13-data
  (->> (io/reader "./resources/day13.txt")
       (line-seq)
       (mapv (partial into []))))

(def test-data
  (->> "/->-\\\n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/"
       (str/split-lines)
       (mapv (partial into []))))

(defn find-carts [track]
  (let [cart-chars #{\< \> \^ \v}
        locs (map-indexed (fn [y row]
                            (map-indexed (fn [x c]
                                           [[x y] (cart-chars c)])
                                         row))
                          track)
        locs (apply concat locs)]
    (->> (filter #(second %) locs)
         (map (fn [[[x y] c]]
                {:x x :y y :c c :i [-1 0 1]})))))
              
(defn just-track [carts track]
  (letfn [(replace-cart [track {:keys [x y c]}]
             (let [r (if (#{\< \>} c) \- \|)]
               (assoc-in track [y x] r)))]  
    (reduce (partial replace-cart) track carts)))

(defn add-carts [carts track]
  (letfn [(add-cart [track {:keys [x y c]}]
            (assoc-in track [y x] c))]  
    (reduce (partial add-cart) track carts)))

(defn print-track! [carts track]
  (let [track (add-carts carts track)]
    (doseq [y (range 0 (count track))]
      (println (apply str (get track y))))))

(defn next-dir
  "Turn is -1, 0, 1"
  [c turn]
  (let [dirs [\< \^ \> \v]
        cur-i (.indexOf dirs c)
        new-i (mod (+ cur-i turn) (count dirs))]  
    (get dirs new-i)))

(defn move-cart [track {:keys [x y c i] :as cart}]
  (let [[x' y'] (case c
                  \> [(inc x) y]
                  \< [(dec x) y]
                  \^ [x (dec y)]
                  \v [x (inc y)])
        next-track (get-in track [y' x'])
        c' (case next-track
             (\- \|) c
             \/ (case c
                  \> \^
                  \< \v
                  \^ \>
                  \v \<)
             \\ (case c
                  \> \v
                  \< \^
                  \^ \<
                  \v \>)
             \+ (next-dir c (first i)))]
    {:x x' :y y' :c c' :i (if (= next-track \+) (take 3 (rest (cycle i))) i)}))

(defn check-collisions [carts]
  (reduce (fn [{:keys [locs collision] :as acc}
               {:keys [x y]}]
            (if (get locs [x y])
              {:locs locs :collision (conj collision [x y])}
              {:locs (conj locs [x y]) :collision collision}))
          {:locs #{} :collision nil}
          carts))

(defn move-all-carts [track carts]
  (loop [acc []
         carts' (sort-by second carts)
         collision nil]
    (if-not (seq carts')
      [acc collision]
      (let [cart (move-cart track (first carts'))
            acc' (conj acc cart)
            collision' (check-collisions (concat carts' acc'))]
        (recur (conj acc cart)
               (sort-by second (rest carts'))
               (if-not (:collision collision') collision (:collision collision')))))))

(defn day13-p1 [data]
  (let [carts (find-carts data)
        track (just-track carts data)]
    (loop [carts carts
           collision nil]
      (if collision 
        collision
        (let [[new-carts new-collision] (move-all-carts track carts)]
          (recur new-carts
                 new-collision))))))

(defn prune-carts [carts collisions]
  (prn :prune-carts :collisions collisions :carts carts)
  (let [remaining-carts (remove #(get collisions (list (:x %) (:y %))) carts)]
    (prn :remaining-carts remaining-carts)
    remaining-carts))

(defn move-prune-all-carts [track carts]
  (loop [acc []
         carts' (sort-by second carts)
         collision #{}]
    (if-not (seq carts')
      [acc collision]
      (let [cart (move-cart track (first carts'))
            acc' (conj acc cart)
            collision' (check-collisions (concat carts' acc'))]
        (recur (conj acc cart)
               (sort-by second (rest carts'))
               (if (:collision collision')
                 (into collision (:collision collision'))
                 collision))))))

(def test-data2
  (->> "/>-<\\\n|   |\n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"
       (str/split-lines)
       (mapv (partial into []))))

(defn day13-p2 [data]
  (let [carts (find-carts data)
        track (just-track carts data)]
    (loop [carts carts
           collision nil
           c 0]
      (if (or (= 20 c)
              (= 1 (count carts))) 
        carts
        (let [[new-carts new-collision] (move-prune-all-carts track carts)
              new-carts (if (empty? new-collision) new-carts
                            (prune-carts new-carts new-collision))
              _ (when (= 1 (count new-carts)) (prn :carts carts))]
          (recur new-carts
                 new-collision
                 c))))))
