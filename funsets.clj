(defn contains? [s] #(s %))
(defn singleton-set [e] #(= % e))

(defmacro combine-sets [f]
  `(fn [s# t#]
     (fn [x#]
       (~f (s# x#) (t# x#)))))
(def union (combine-sets or))
(def intersect (combine-sets and))
(def diff (combine-sets #(and %1 (not %2))))
(def filter intersect)

(def bound 1000)

(defn forall [s p]
  (empty? (for [x (range (- bound) (inc bound)) :when (and (s x) (not (p x)))] x)))
(defn exists [s p]
  (not (forall s (complement p))))

(defn map [s f]
  (fn [y]
    (not (empty? (for [x (range (- bound) (inc bound)) :when (and (= (f x) y) (s x))] x)))))

(defn set-string [s]
  (let [strs (for [x (range (- bound) (inc bound)) :when (s x)] (str x))]
    (clojure.string/join ", " strs)))
(defn print-set [s]
  (print (set-string s)))
