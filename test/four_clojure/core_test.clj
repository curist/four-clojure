(ns four-clojure.core-test
  (:require [clojure.test :refer :all]
            [four-clojure.core :as core]))


(deftest test-p86
  (let [f core/p86]
    (testing "all the things"
      (is (= (f 7) true))
      (is (= (f 986543210) true))
      (is (= (f 2) false))
      (is (= (f 3) false)))))

(deftest test-p115
  (let [__ core/p115]
    (testing "all the things"
      (is (= true (__ 11)))
      (is (= true (__ 121)))
      (is (= false (__ 123)))
      (is (= true (__ 0)))
      (is (= false (__ 88099)))
      (is (= true (__ 89098)))
      (is (= true (__ 89089)))
      (is (= true (__ 89089)))
      (is (= (take 20 (filter __ (range)))
             [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])))))

(deftest test-p98
  (let [__ core/p98]
    (testing "equivalence classes"
      (is (= (__ #(* % %) #{-2 -1 0 1 2})
             #{#{0} #{1 -1} #{2 -2}}))
      (is (= (__ #(rem % 3) #{0 1 2 3 4 5 })
             #{#{0 3} #{1 4} #{2 5}}))
      (is (= (__ identity #{0 1 2 3 4})
             #{#{0} #{1} #{2} #{3} #{4}}))
      (is (= (__ (constantly true) #{0 1 2 3 4})
             #{#{0 1 2 3 4}})))))

(deftest test-p105
  (let [__ core/p105]
    (testing "identify keys and values"
      (is (= {} (__ [])))
      (is (= {:a [1]} (__ [:a 1])))
      (is (= {:a [1], :b [2]} (__ [:a 1, :b 2])))
      (is (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))))))

(deftest test-p137
  (let [__ core/p137]
    (testing "digits and bases"
      (is (= [1 2 3 4 5 0 1] (__ 1234501 10)))
      (is (= [0] (__ 0 11)))
      (is (= [1 0 0 1] (__ 9 2)))
      (is (= [1 0] (let [n (rand-int 100000)](__ n n))))
      (is (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))))))

(deftest test-p110
  (let [__ core/p110]
    (testing "sequence of pronunciations"
      (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1]))))
      (is (= [3 1 2 4] (first (__ [1 1 1 4 4]))))
      (is (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6)))
      (is (= 338 (count (nth (__ [3 2]) 15)))))))

(deftest test-p144
  (let [__ core/p144]
    (testing "oscilrate"
      (is (= (take 3 (__ 3.14 int double)) [3.14 3 3.0]))
      (is (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
      (is (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))))

(deftest test-p108
  (let [__ core/p108]
    (testing "lazy searching"
      (is (= 3 (__ [3 4 5])))
      (is (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
      (is (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13])))
      (is (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
                    (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                    (iterate inc 20)))) ;; at least as large as 20
      )))

(deftest test-p158
  (let [__ core/p158]
    (testing "decurry"
      (is (= 10 ((__ (fn [a]
                       (fn [b]
                         (fn [c]
                           (fn [d]
                             (+ a b c d))))))
                 1 2 3 4)))
      (is (= 24 ((__ (fn [a]
                       (fn [b]
                         (fn [c]
                           (fn [d]
                             (* a b c d))))))
                 1 2 3 4)))
      (is (= 25 ((__ (fn [a]
                       (fn [b]
                         (* a b))))
                 5 5)))
      (is (= 25 ((__ (fn [a]
                       (fn [b]
                         (* a b))))
                 5 5))))))

(deftest test-p93
  (let [__ core/p93]
    (testing "partially flatten a sequence"
      (is (= (__ [["Do"] ["Nothing"]])
             [["Do"] ["Nothing"]]))
      (is (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
             [[:a :b] [:c :d] [:e :f]]))
      (is (= (__ '((1 2)((3 4)((((5 6)))))))
             '((1 2)(3 4)(5 6)))))))

(deftest test-p114
  (let [__ core/p114]
    (testing "global take-while"
      (is (= [2 3 5 7 11 13]
              (__ 4 #(= 2 (mod % 3))
                  [2 3 5 7 11 13 17 19 23])))
      (is (= ["this" "is" "a" "sentence"]
              (__ 3 #(some #{\i} %)
                  ["this" "is" "a" "sentence" "i" "wrote"])))
      (is (= ["this" "is"]
             (__ 1 #{"a"}
                 ["this" "is" "a" "sentence" "i" "wrote"]))))))

