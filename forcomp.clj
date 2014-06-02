(defn word-occurrences [s]
  (sort-by first (map (fn [x] [(first x) (count (second x))])
                      (group-by identity (.toLowerCase (apply str s))))))

(defn sentence-occurrences [seq]
  (word-occurrences (reduce concat seq)))

(def dictionary (clojure.string/split-lines (slurp "linuxwords.txt")))

(def dictionary-by-occurrences (group-by word-occurrences dictionary))

(defn anagrams-by-occurrences [occ]
  (get dictionary-by-occurrences occ))

(defn word-anagrams [word]
  (anagrams-by-occurrences (word-occurrences word)))

(defn combinations [occ]
  (if (empty? occ)
    '(())
    (let [x (first occ)]
      (for [i (range (inc (second x)))
            tail (combinations (rest occ))]
        (if (= i 0)
          tail
          (cons [(first x) i] tail))))))

(defn subtract [x y]
  (letfn [(update-map [a b]
            (let [n (get a (first b))]
              (if (= n (second b))
                (dissoc a (first b))
                (assoc a (first b) (- n (second b))))))]
    (sort-by first (reduce update-map (into {} x) y))))

(defn sentence-anagrams [sentence]
  (letfn [(rec [occ]
            (if (empty? occ)
              '(())
              (for [comb (combinations occ)
                    head (anagrams-by-occurrences comb)
                    tail (rec (subtract occ comb))]
                (cons head tail))))]
    (rec (sentence-occurrences sentence))))
