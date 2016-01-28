(ns four-clojure.core
  (:gen-class))

(defn reload []
  (use 'four-clojure.core :reload))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn p86 [n]
  (let [sqrt (fn [x] (* x x))
        sq-sum (fn [sum x]
                 (if-not (> x 0)
                   sum
                   (recur (+ sum (sqrt (mod x 10)))
                          (int (/ x 10)))))
        loop-x (fn [s x]
                 (if (s x)
                   x
                   (recur (conj s x) (sq-sum 0 x))))]
    (= 1 (loop-x #{} n))))


(defn p115 [n]
  (let [s (str n)
        l (-> s count (/ 2) int)
        ch->int #(- (int %) 48)
        sum (fn [xs] (reduce #(+ %1 (ch->int %2)) 0 xs))]
    (= (sum (take l s))
       (sum (take-last l s)))))

(defn p98 [f s]
  (->> s (group-by f) vals (map set) set))

(defn p105' [xs]
  (let [f (fn [m prev-k [x & xs]]
            (if (nil? x)
              m
              (if (keyword? x)
                (recur (assoc m x []) x xs)
                (recur (update m prev-k conj x) prev-k xs))))]
    (f {} nil xs)))

(defn p105 [xs]
  (let [r (fn [m v]
            (if (number? v)
              (update-in m [:r (:k m)] conj v)
              (-> m
                  (assoc :k v)
                  (assoc-in [:r v] []))))]
    (:r (reduce r {:r {}} xs))))

(defn p137' [max-x x]
  (let [r (->> (take 7 (iterate #(* % x) 1N))
               (mapv #(-> max-x (/ %) (rem x) int))
               reverse
               (split-with zero?)
               second)]
    (if (empty? r)
      [0] r)))

(defn p137 [max-x x]
  (loop [r nil n max-x]
    (if (zero? n)
      (or r [0])
      (recur (cons (mod n x) r) (quot n x)))))

(defn p110 [xs]
  (rest (iterate
          (fn [xs]
            (->> xs
                 (partition-by identity)
                 (mapcat #((juxt count first) %))))
          xs)))


(defn p144' [x f & fs]
  (let [v (f x)
        next-args (concat [v] fs [f])]
    (lazy-seq (cons x (apply p144' next-args)))))

(defn p144 [x & fs]
  (reductions
    #(%2 %)
    x (cycle fs)))

(defn p108' [& o-colls]
  (loop [colls o-colls]
    (if (apply = (map first colls))
      (ffirst colls)
      (let [clls (sort-by first colls)
            cls (concat [(rest (first clls))] (rest clls))]
        (recur cls)))))

(defn p108 [& colls]
  (if (apply = (map first colls))
    (ffirst colls)
    (let [clls (sort-by first colls)
          cls (concat [(rest (first clls))] (rest clls))]
      (recur cls))))

(defn p158 [f]
  (fn [& args]
    (reduce #(% %2) f args)))

(defn p93 [x]
  (let [seq-atom? (comp not coll? first)]
    (filter seq-atom?
            (rest (tree-seq (complement seq-atom?) seq x)))))


(def p114' #(take ({4 6 3 4 1 2} %) %3))

(defn p114 [n pred xs]
  (take (->> xs
             (map-indexed vector)
             (filter #(pred (second %)))
             (take n) last first) xs))
