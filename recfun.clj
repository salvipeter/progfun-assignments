;;; Assignment 1 in Clojure

(defn pascal-naive [c r]
  (if (or (= c 0) (= c r))
    1
    (+ (pascal-naive (dec c) (dec r))
       (pascal-naive c (dec r)))))

;;; David verziojanak parafrazisa
(defn pascal-david [c r]
  (letfn [(col [xs]
            (if (empty? (rest xs))
              xs
              (let [x (first xs)]
                (cons x (col (cons (+ x (second xs)) (nnext xs)))))))]
    (let [start (take (- r c -1) (repeat 1))]
      (last (nth (iterate col start) c)))))

;;; Gyors verzio
(defn pascal [c r]
  (letfn [(rec [n k acc]
            (if (> k c)
              acc
              (recur (dec n) (inc k) (/ (* acc n) k))))]
    (rec r 1 1)))

(defn main []
  (println "Pascal's Triangle")
  (dotimes [row 11]
    (dotimes [col (inc row)]
      (print (format "%d " (pascal col row))))
    (println)))

(defn balance [str]
  (letfn [(rec [str open]
            (cond (empty? str) (= open 0)
                  (= (first str) \() (rec (rest str) (inc open))
                  (= (first str) \)) (and (> open 0) (rec (rest str) (dec open)))
                  :else (recur (rest str) open)))]
    (rec str 0)))

(defn count-change [money coins]
  (cond (zero? money) 1
        (empty? coins) 0
        (<= (first coins) money) (+ (count-change (- money (first coins)) coins)
                                    (count-change money (rest coins)))
        :else (recur money (rest coins))))
