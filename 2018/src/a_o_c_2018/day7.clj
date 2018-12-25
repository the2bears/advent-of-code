(ns a-o-c-2018.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def day7-data
  (->> (io/reader "./resources/day7.txt")
       (line-seq)))

(def test-data
  (->> (str/split-lines
         "Step C must be finished before step A can begin.
         Step C must be finished before step F can begin.
         Step A must be finished before step B can begin.
         Step A must be finished before step D can begin.
         Step B must be finished before step E can begin.
         Step D must be finished before step E can begin.
         Step F must be finished before step E can begin.")
       (map str/trim)))

(defn parse-all [data]
  (->> data
       (map #(str/split % #" "))
       (map (juxt second #(nth % 7)))))

(def test-tuples (parse-all test-data))

(defn all-nodes [tuples]
  (reduce #(into %1 %2) #{} tuples))  

(def nodes (all-nodes test-tuples))

(defn dependency-tree [tuples]
  (reduce (fn[acc [dee der]]
            (update acc der (fnil conj #{}) dee))
          {}
          tuples))

(def tree (dependency-tree test-tuples))

(defn has-deps? [node tree]
  (let [deps (get tree node #{})]
    (seq deps)))

(has-deps? "C" tree)

(defn sorted-no-deps [nodes tree]
  (->> nodes
       (reduce #(if-not (has-deps? %2 tree) (conj %1 %2) %1) [])
       (sort)))

(defn mark-complete [node tree]
  (let [ders (keys tree)]
    (reduce #(update %1 %2 disj node) tree ders)))

(defn day7-p1 [data]
  (let [tuples (parse-all data)
        dep-tree (dependency-tree tuples)
        nodes (all-nodes tuples)]
    (loop [acc []
           ns nodes
           dep-tree dep-tree]
      (if (empty? ns)
        (apply str acc)
        (let [next-node (first (sorted-no-deps ns dep-tree))]
          (recur (conj acc next-node)
                 (disj ns next-node)
                 (mark-complete next-node dep-tree)))))))
;;"LFMNJRTQVZCHIABKPXYEUGWDSO"

;;Part 2
;; Check for completed tasks, if completed add to result,
;;   clear from tree, clear from task list
;;   free worker
;; Get tasks that have no dependencies, sort them
;; Get available workers
;; Assign tasks in order to workers
;; Remove assigned tasks from nodes used to generate availabe task list
;; Tick
;; recur

(defn make-workers [n]
  (->> (map (fn[id] [id {}]) (range n))
       (into {})))

(defn available-workers [workers]
  (remove #(get-in workers [% :task]) (keys workers)))

(defn node->duration [s]
  (let [c (first s)]
    (->> (- (int c) (int \A))
         (+ 1 60))))

(defn assign-task [t task worker workers]
  (update workers worker assoc :task task :done-by (+ t (node->duration task))))

(defn completed-tasks [t workers]
  (reduce #(if (= t (get-in workers [%2 :done-by]))
             (conj %1 (get-in workers [%2 :task]))
             %1)
          #{} (keys workers)))

(defn clear-task [task workers]
  (reduce #(if (= task (get-in workers [%2 :task]))
             (assoc %1 %2 {})
             %1)
          workers
          (keys workers)))

(defn assign-all-tasks
  "Assigns tasks to available workers, returns updated [workers nodes]"
  [t atasks aworkers ws ns]
  (let [atasks (if (> (count atasks) (count aworkers))
                 (take (count aworkers) atasks)
                 atasks)
        aworkers (if (> (count aworkers) (count atasks))
                   (take (count atasks) aworkers)
                   aworkers)
        combos (map vector atasks aworkers)]
    (vector (reduce #(assign-task t (first %2) (second %2) %1) ws combos)
            (reduce #(disj %1 %2) ns atasks))))     
  
  

(defn day7-p2 [data nw]
  (let [tuples (parse-all data)
        dep-tree (dependency-tree tuples)
        nodes (all-nodes tuples)
        workers (make-workers nw)]
    (loop [acc []
           tick 0
           dt dep-tree
           ns nodes
           ws workers]
      (let [completed-tasks (completed-tasks tick ws)
            new-acc (reduce conj acc completed-tasks)
            new-dt (reduce #(mark-complete %2 %1) dt completed-tasks)
            new-ws (reduce #(clear-task %2 %1) ws completed-tasks)
            available-tasks (sorted-no-deps ns new-dt)
            aw (available-workers new-ws)
            [new-ws new-ns] (assign-all-tasks tick available-tasks aw new-ws ns)
            new-aw (available-workers new-ws)]
        (if (= nw (count new-aw)) ;;;break when all workers are idle
          tick
          (recur new-acc (inc tick) new-dt new-ns new-ws))))))
;;1180











