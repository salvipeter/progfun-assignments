;;; Code tree structure:
;;; {:node :leaf :char \a :weight 1}
;;; {:node :fork :left _ :right _ :chars (\a \b) :weight 2}

(defn leaf? [node] (= (:node node) :leaf))
(defn fork? [node] (not (leaf? node)))

(defn weight [node] (:weight node))
(defn chars [node] (if (leaf? node) (list (:char node)) (:chars node)))

(defn make-code-tree [l r]
  {:node :fork :left l :right r
   :chars (concat (chars l) (chars r)) :weight (+ (weight l) (weight r))})

(defn times [seq]
  (if (empty? seq)
    ()
    (let [x (first seq)]
      (cons [x (count (filter #(= x %) seq))]
            (times (filter #(not= x %) (rest seq)))))))

(defn make-ordered-leaf-list [freq]
  (let [sorted (sort #(< (second %1) (second %2)) freq)]
    (map (fn [x] {:node :leaf :char (first x) :weight (second x)}) sorted)))

(defn singleton? [trees]
  (empty? (rest trees)))

(defn combine [trees]
  (let [x (first trees)
        y (second trees)
        xy (make-code-tree x y)
        tail (rest (rest trees))]
    (letfn [(rec [seq]
              (if (empty? seq)
                (list xy)
                (let [t (first seq)]
                  (if (< (weight t) (< weight xy))
                    (cons t (rec (rest seq)))
                    (cons xy (cons t (rest seq)))))))]
      (rec trees))))

(defn until [s c]
  (fn [t]
    (if (s t)
      t
      ((until s c) (c t)))))

(defn create-code-tree [seq]
  (first ((until singleton? combine) (make-ordered-leaf-list (times seq)))))

(defn decode [tree bits]
  (letfn [(rec [subtree bits]
            (cond (leaf? subtree) (cons (:char subtree) (rec tree bits))
                  (empty? bits) ()
                  :else (if (zero? (first bits))
                          (rec (:left subtree) (rest bits))
                          (rec (:right subtree) (rest bits)))))]
    (rec tree bits)))

(def french-code {:node :fork :left {:node :fork :left {:node :fork :left {:node :leaf :char \s :weight 121895} :right {:node :fork :left {:node :leaf :char \d :weight 56269} :right {:node :fork :left {:node :fork :left {:node :fork :left {:node :leaf :char \x :weight 5928} :right {:node :leaf :char \j :weight 8351} :chars "xj" :weight 14279} :right {:node :leaf :char \f :weight 16351} :chars "xjf" :weight 30630} :right {:node :fork :left {:node :fork :left {:node :fork :left {:node :fork :left {:node :leaf :char \z :weight 2093} :right {:node :fork :left {:node :leaf :char \k :weight 745} :right {:node :leaf :char \w :weight 1747} :chars "kw" :weight 2492} :chars "zkw" :weight 4585} :right {:node :leaf :char \y :weight 4725} :chars "zkwy" :weight 9310} :right {:node :leaf :char \h :weight 11298} :chars "zkwyh" :weight 20608} :right {:node :leaf :char \q :weight 20889} :chars "zkwyhq" :weight 41497} :chars "xjfzkwyhq" :weight 72127} :chars "dxjfzkwyhq" :weight 128396} :chars "sdxjfzkwyhq" :weight 250291} :right {:node :fork :left {:node :fork :left {:node :leaf :char \o :weight 82762} :right {:node :leaf :char \l :weight 83668} :chars "ol" :weight 166430} :right {:node :fork :left {:node :fork :left {:node :leaf :char \m :weight 45521} :right {:node :leaf :char \p :weight 46335} :chars "mp" :weight 91856} :right {:node :leaf :char \u :weight 96785} :chars "mpu" :weight 188641} :chars "olmpu" :weight 355071} :chars "sdxjfzkwyhqolmpu" :weight 605362} :right {:node :fork :left {:node :fork :left {:node :fork :left {:node :leaf :char \r :weight 100500} :right {:node :fork :left {:node :leaf :char \c :weight 50003} :right {:node :fork :left {:node :leaf :char \v :weight 24975} :right {:node :fork :left {:node :leaf :char \g :weight 13288} :right {:node :leaf :char \b :weight 13822} :chars "gb" :weight 27110} :chars "vgb" :weight 52085} :chars "cvgb" :weight 102088} :chars "rcvgb" :weight 202588} :right {:node :fork :left {:node :leaf :char \n :weight 108812} :right {:node :leaf :char \t :weight 111103} :chars "nt" :weight 219915} :chars "rcvgbnt" :weight 422503} :right {:node :fork :left {:node :leaf :char \e :weight 225947} :right {:node :fork :left {:node :leaf :char \i :weight 115465} :right {:node :leaf :char \a :weight 117110} :chars "ia" :weight 232575} :chars "eia" :weight 458522} :chars "rcvgbnteia" :weight 881025} :chars "sdxjfzkwyhqolmpurcvgbnteia" :weight 1486387})

(def secret '(0 0 1 1 1 0 1 0 1 1 1 0 0 1 1 0 1 0 0 1 1 0 1 0 1 1 0 0 1 1 1 1 1 0 1 0 1 1 0 0 0 0 1 0 1 1 1 0 0 1 0 0 1 0 0 0 1 0 0 0 1 0 1))

(def decoded-secret (decode french-code secret))

(defn encode [tree text]
  (letfn [(rec [tree c]
            (if (leaf? tree)
              ()
              (if (some #(= c %) (chars (:left tree)))
                (cons 0 (rec (:left tree) c))
                (cons 1 (rec (:right tree) c)))))]
    (reduce concat (map #(rec tree %) text))))

(defn code-bits [table]
  (fn [char]
    (second (first (drop-while #(not= (first %) char) table)))))

(defn merge-code-tables [a b]
  (letfn [(prefix [p] (fn [x] [(first x) (cons p (second x))]))]
    (concat (map (prefix 0) a) (map (prefix 1) b))))

(defn convert [tree]
  (if (leaf? tree)
    (list [(:char tree) ()])
    (merge-code-tables (convert (:left tree)) (convert (:right tree)))))

(defn quick-encode [tree text]
  (let [table (convert tree)]
    (reduce concat (map (code-bits table) text))))
