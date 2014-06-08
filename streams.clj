;;; Global variable, set by `init-level'
(def level (ref {}))

(defn pos-dx [[x y] d]
  [(+ x d) y])

(defn pos-dy [[x y] d]
  [x (+ y d)])

(defn block-dx [[p q] [d1 d2]]
  [(pos-dx p d1) (pos-dx q d2)])

(defn block-dy [[p q] [d1 d2]]
  [(pos-dy p d1) (pos-dy q d2)])

;;; 1a
(defn terrain-function [terrain]
  (fn [[x y]]
    (if (or (< x 0) (< y 0) (>= x (count terrain)) (>= y (count (first terrain))))
      false
      (not= (nth (nth terrain x) y) \-))))

;;; 1b
(defn find-char [c terrain]
  (let [x (first (first (filter (fn [coll] (some #(= c %) (second coll)))
                                (map vector (iterate inc 0) terrain))))
        y (first (first (filter #(= c (second %))
                                (map vector (iterate inc 0) (nth terrain x)))))]
    [x y]))

;;; 2a
(defn standing? [[p q]]
  (= p q))

;;; 2b
(defn legal? [[p q]]
  (let [terrain (:terrain @level)]
    (and (terrain p) (terrain q))))

;;; 2c
(defn start-block []
  (let [sp (:start-pos @level)]
    [sp sp]))

(defn movement [f standing in-x in-y]
  (fn [[[ px _] [qx _] :as b]]
    (cond (standing? b) (f b standing)
          (= px qx)     (f b in-x)
          :else         (f b in-y))))
(def left  (movement block-dy [-2 -1] [-1 -2] [-1 -1]))
(def right (movement block-dy [ 1  2] [ 2  1] [ 1  1]))
(def up    (movement block-dx [-2 -1] [-1 -1] [-1 -2]))
(def down  (movement block-dx [ 1  2] [ 1  1] [ 2  1]))

;;; 3a
(defn neighbors [block]
  (list [(left block) :left] [(right block) :right] [(up block) :up] [(down block) :down]))

;;; 3b
(defn legal-neighbors [block]
  (filter #(legal? (first %)) (neighbors block)))

;;; 4a
(defn done? [[p _ :as block]]
  (and (standing? block) (= p (:goal @level))))

;;; 4b
(defn neighbors-with-history [block history]
  (map (fn [n] [(first n) (cons (second n) history)])
       (legal-neighbors block)))

;;; 4c
(defn new-neighbors-only [visited coll]
  (filter #(not (visited (first %))) coll))

;;; 4d
(defn from [coll visited]
  (if (empty? coll)
    ()
    (let [next
          (mapcat #(new-neighbors-only visited (neighbors-with-history (first %) (second %)))
                  coll)]
      (lazy-cat coll (from next (clojure.set/union visited (set (map first coll))))))))

;;; 4e/1
(defn paths-from-start []
  (from (list [(start-block) ()]) {}))

;;; 4e/2
(defn paths-to-goal []
  (filter #(done? (first %)) (paths-from-start)))

;;; 4e/3
(defn solution []
  (let [paths (paths-to-goal)]
    (if (empty? paths)
      ()
      (second (first paths)))))

;;; Test

(def level1
  ["ooo-------"
   "oSoooo----"
   "ooooooooo-"
   "-ooooooooo"
   "-----ooToo"
   "------ooo-"])

(defn init-level [coll]
  (dosync
   (alter level assoc :terrain (terrain-function coll))
   (alter level assoc :start-pos (find-char \S coll))
   (alter level assoc :goal (find-char \T coll))))

;;; (init-level level1)
;;; (solution)
