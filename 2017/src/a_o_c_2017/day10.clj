(ns a-o-c-2017.day10)

(def data [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190])
(def data2 "97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190")
(def std-suffix [17, 31, 73, 47, 23])

(defn- reverse-n [s n]
  (let [first-n (take n s)
        the-rest (drop n s)]
    (concat (reverse first-n) the-rest)))

(defn- from-c [s c]
  (concat (drop c s) (take c s)))

(defn- to-c [s c]
  (from-c s (- (count s) c)))

(defn- next-pos [s current length skip]
  (mod (+ current length skip) (count s)))

(defn- rev-n-from-c [s n c]
  (let [pre-rev (from-c s c)
        rev (reverse-n pre-rev n)
        post-rev (to-c rev c)]
    post-rev))


                                        ;8536 is the correct answer
(defn- part-1*  
  ([circle data]
   (part-1* circle data 0 0))
  ([circle data cur-pos skip]
   (loop [s circle
          lengths data
          cur-pos cur-pos
          skip skip]
     (if (seq lengths)
       (let [length (first lengths)]
         (recur (rev-n-from-c s length cur-pos)
                (rest lengths)
                (next-pos s cur-pos length skip)
                (inc skip)))
       {:result (* (first s) (second s))
        :s s
        :cur-pos cur-pos
        :skip skip}))))  

(defn part-1 [circle data]
  (part-1* circle data))

(defn to-ascii [s]
  (let [chars (seq s)]
    (map int chars)))    

(defn p1-n-times [circle lengths n]
  (loop [times n
         circle circle
         cur-pos 0
         skip 0]
    (if (= 0 times)
      circle
      (let [result (part-1* circle lengths cur-pos skip)]
        ;(prn circle)
        (recur (dec times)
               (:s result)
               (:cur-pos result)
               (:skip result))))))   

(defn to-hex [i]
  (let [hex-str (Integer/toHexString i)]
    (if (= 2 (count hex-str))
      hex-str
      (str "0" hex-str))))


                                        ;aff593797989d665349efe11bb4fd99b is the correct answer
(defn part-2 [circle data]
  (let [lengths (concat (to-ascii data) std-suffix)
        circle (p1-n-times circle lengths 64)]
    (->> circle
         (partition 16)
         (map #(apply bit-xor %))
         (map to-hex)
         (apply str))))
