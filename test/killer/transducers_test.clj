(ns killer.transducers-test
  (:require [killer.transducers :refer :all]
            [clojure.test :refer :all]))

(deftest test-transducers

  ;; higher order transducers
  
  (testing "esp-apply"
    (is
     (let [expected [1 2 3 4]
           actual (sequence (esp-apply +) [[1][1 1][1 2][2 2]])]
       (= actual expected))))

  ;; maths - simple
  
  (testing "esp-+"
    (is
     (let [expected [1 3 6 10 15]
           actual (sequence (esp-+) [1 2 3 4 5])]
       (= actual expected))))

  (testing "esp-*"
    (is
     (let [expected [1 2 6 24 120]
           actual (sequence (esp-*) [1 2 3 4 5])]
       (= actual expected))))

  ;; sequences - building
  
  (testing "esp-vector"
    (is
     (let [expected [[1] [1 2] [1 2 3] [1 2 3 4]]
           actual (sequence (esp-vector) '(1 2 3 4))]
       (= actual expected))))

  (testing "esp-take-last"
    (is
     (let [expected [[1] [1 2] [1 2 3] [2 3 4] [3 4 5] [4 5 6] [5 6 7] [6 7 8] [7 8 9]]
           actual (map sequence (sequence (esp-take-last 3) [1 2 3 4 5 6 7 8 9]))]
       (= actual expected))))
  
  (testing "esp-hash-set"
    (is
     (let [expected [#{1} #{1 2} #{1 2} #{1 2 3} #{1 2 3}]
           actual (sequence (esp-hash-set) [1 2 1 3 2])]
       (= actual expected))))

  (testing "esp-hash-map"
    (is
     (let [expected [{:a 1} {:a 1 :b 2} {:a 3 :b 2}{:a 3 :b 2 :c 4}]
           actual (sequence (esp-hash-map) [[:a 1][:b 2][:a 3][:c 4]])]
       (= actual expected))))

  (testing "esp-hash-map-by - length:string"
    (is
     (let [expected [{1 "a"}
                     {1 "a" 2 "bb"}
                     {1 "a" 2 "bb" 3 "ccc"}
                     {1 "a" 2 "bb" 3 "ccc" 4 "dddd"}]
           actual (sequence (esp-hash-map-by (fn [s] (.length s))) ["a" "bb" "ccc" "dddd"])]
       (= actual expected))))

  ;; sequences -  operations

  ;; sequences - transformations

  (testing "esp-group-by - length:strings"
    (is
     (let [expected [{1 ["a"]}
                     {1 ["a" "b"]}
                     {1  ["a" "b"] 2 ["aa"]}
                     {1  ["a" "b"] 2 ["aa" "bb"]}]
           actual (sequence (esp-group-by (fn [s] (.length s)) conj []) ["a" "b" "aa" "bb"])]
       (= actual expected))))

  (testing "esp-frequencies"
    (is
     (let [expected [{1 1}
                     {1 1 2 1}
                     {1 1 2 1 3 1}
                     {1 1 2 1 3 1 4 1}
                     {1 2 2 1 3 1 4 1}
                     {1 2 2 2 3 1 4 1}
                     {1 2 2 2 3 2 4 1}
                     {1 3 2 2 3 2 4 1}
                     {1 3 2 3 3 2 4 1}
                     {1 4 2 3 3 2 4 1}]
           actual (sequence (esp-frequencies) [1 2 3 4 1 2 3 1 2 1])]
       (= actual expected))))

  (testing "esp-pie-chart"
    (is
     (let [expected [{1 100}
                     {1 50, 2 50}
                     {1 100/3, 2 100/3, 3 100/3}
                     {1 25, 2 25, 3 25, 4 25}
                     {1 40, 2 20, 3 20, 4 20}
                     {1 100/3, 2 100/3, 3 50/3, 4 50/3}
                     {1 200/7, 2 200/7, 3 200/7, 4 100/7}
                     {1 75/2, 2 25, 3 25, 4 25/2}
                     {1 100/3, 2 100/3, 3 200/9, 4 100/9}
                     {1 40, 2 30, 3 20, 4 10}]
           actual (sequence (comp (esp-frequencies)(esp-pie-chart 100)) [1 2 3 4 1 2 3 1 2 1])]
       (= actual expected))))
  
  ;; maths - more complex

  (testing "esp-mean"
    (is
     (let [expected [1 3/2 2 5/2 3]
           actual (sequence (esp-mean) [1 2 3 4 5])]
       (= actual expected))))

  (testing "esp-median"
    (is
     (let [expected [1 2 2 5/2 3]
           actual (sequence (esp-median) [1 3 2 5 4])]
       (= actual expected))))

  (testing "esp-mode"
    (is
     (let [expected [1 nil nil nil 1 nil nil 1 nil 1]
           actual (sequence (esp-mode) [1 2 3 4 1 2 3 1 2 1])]
       (= actual expected))))

  )

;;------------------------------------------------------------------------------

;; what about :
;; - versions
;; - metadata
;; - join
;; - union
;; - etc...
;; ??
