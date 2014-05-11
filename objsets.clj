(defn show [tweet]
  (format "User: %s\nText: %s [%d]" (:user tweet) (:text tweet) (:retweets tweet)))

(defn contains [set tweet]
  (when (not (empty? set))
    (let [c (compare (:text tweet) (:text (:elem set)))]
      (cond (neg? c) (recur (:left set) tweet)
            (pos? c) (recur (:right set) tweet)
            :else true))))

(defn incl [set tweet]
  (if (empty? set)
    {:elem tweet :left {} :right {}}
    (let [c (compare (:text tweet) (:text (:elem set)))]
      (cond (neg? c) (assoc set :left (incl (:left set) tweet))
            (pos? c) (assoc set :right (incl (:right set) tweet))
            :else set))))

;;; Exercise 2
(defn union [s t]
  (if (empty? s)
    t
    (union (:left s) (union (:right s) (incl t (:elem s))))))

(defn remove [set tweet]
  (if (empty? set)
    {}
    (let [c (compare (:text tweet) (:text (:elem set)))]
      (cond (neg? c) (assoc set :left (remove (:left set) tweet))
            (pos? c) (assoc set :right (remove (:right set) tweet))
            :else (union (:left set) (:right set))))))

;;; Exercise 1
(defn filter [pred set]
  (letfn [(rec [set acc]
            (if (empty? set)
              acc
              (let [e (:elem set)
                    next (if (pred e) (incl acc e) acc)]
                (rec (:right set) (rec (:left set) next)))))]
    (rec set {})))

;;; Exercise 3
(defn most-retweeted [set]
  (letfn [(rec [set max]
            (if (empty? set)
              max
              (let [e (:elem set)
                    best (if (> (:retweets e) (:retweets max)) e max)]
                (rec (:left set) (rec (:right set) best)))))]
    (rec set (:elem set))))
(defn descending-by-retweet [set]
  (if (empty? set)
    ()
    (let [max (most-retweeted set)]
      (cons max (descending-by-retweet (remove set max))))))

(load "objsets-data")
(def sites [gizmodo tech-crunch engadget amazondeals cnet gadgetlab mashable])
(def all-tweets
  (letfn [(tweets->set [lst]
            (if (empty? lst)
              {}
              (incl (tweets->set (rest lst)) (first lst))))]
    (reduce union {} (map tweets->set sites))))

(def google ["android" "Android" "galaxy" "Galaxy" "nexus" "Nexus"])
(def apple ["ios" "iOS" "iphone" "iPhone" "ipad" "iPad"])

;;; Exercise 4
(defn in-list [lst] (fn [tweet] (some #(.contains (:text tweet) %) lst)))
(def google-tweets (filter (in-list google) all-tweets))
(def apple-tweets (filter (in-list apple) all-tweets))
(def trending (descending-by-retweet (union google-tweets apple-tweets)))

(defn main []
  (doseq [tweet trending]
    (println (show tweet))))
