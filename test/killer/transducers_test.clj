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
           actual (sequence (esp-hash-map-by (fn [^String s] (.length s))) ["a" "bb" "ccc" "dddd"])]
       (= actual expected))))

  ;; sequences -  operations

  ;; sequences - transformations

  (testing "esp-group-by - length:strings"
    (is
     (let [expected [{1 ["a"]}
                     {1 ["a" "b"]}
                     {1  ["a" "b"] 2 ["aa"]}
                     {1  ["a" "b"] 2 ["aa" "bb"]}]
           actual (sequence (esp-group-by (fn [^String s] (.length s)) conj []) ["a" "b" "aa" "bb"])]
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

;;------------------------------------------------------------------------------
;; a transducer operates on a stream of values
;; a hyperducer operates on a stream of tuples of an optional deletion and optional addition

(def ->hyperduction (map (fn [e] [[][e]])))
(def <-hyperduction (mapcat second))    ;can we just throw away deletions ?

(deftest test-hyperduction

  (testing "->hyperduction"
    (is
     (=
      [[[][nil]] [[][1]] [[][2]] [[][3]]]
      (sequence ->hyperduction [nil 1 2 3]))))

  (testing "<-hyperduction"
    (is
     (= [nil 1 2 3]
        (sequence <-hyperduction [[[][nil]] [[][1]] [[][2]] [[][3]]]))))

  (testing "round-trip hyperduction"
    (is
     (= [nil 1 2 3]
        (sequence (comp ->hyperduction <-hyperduction) [nil 1 2 3]))))

  ;; ok - now what can we do with one...

  ;; we can implement a sliding window - with values both entering one end and leaving at the other - as a hyper/transducer

  (defn- superducer
    [f i]
    (fn [xf]
      (let [accumulator (volatile! i)]
        (fn
          ([] (xf))
          ([result] (xf result))
          ([result input] (xf result (vswap! accumulator f input)))))))
  
  (testing "superduction"
    (is
     (= [1 3 6]
        (sequence (superducer + 0) [1 2 3]))))

  (defn- hyperducer
    [f i]
    (fn [xf]
      (let [accumulator (volatile! i)]
        (fn
          ([] (xf))
          ([result] (xf result))
          ([result input]
           (let [[tmp output] (f @accumulator input)]
             (vreset! accumulator tmp)
             (xf result output)))))))

  (defn vor [a b] (if (empty? a) b (first a)))
  
  (defn commutative-hyperducer [fa fd i]
    (hyperducer
     (fn [acc [maybe-d maybe-a]]
       (let [new-acc (fa (fd acc (vor maybe-d i)) (vor maybe-a i))]
         [new-acc [[] [new-acc]]]))
     i))
  
  (testing "commutative hyperduction"
    (is
     (= [[[][1]] [[][3]] [[][5]] [[][7]] [[][9]]]
        (sequence
         (commutative-hyperducer + - 0)
         [[[][1]] [[][2]] [[1][3]] [[2][4]] [[3][5]]]))))

  (defn hd-take-last
    [n]
    (hyperducer
     (fn [old-a [_[v]]]
       (let [[new-a deletion]
             (if (= (count old-a) n)
               [(pop old-a) [(first old-a)]]
               [old-a []])]
         [(conj new-a v) [deletion [v]]]))
     (clojure.lang.PersistentQueue/EMPTY)))

  (testing "hd-take-last"
    (is
     (=
      [[[][nil]] [[][1]] [[nil][2]] [[1][3]]]
      (sequence (comp ->hyperduction (hd-take-last 2)) [nil 1 2 3])
      )
     ))

  (defn hd-sum [] (commutative-hyperducer + - 0))
  
  (testing "the superduction of the SUM of a sliding window size 2..."
    (is
     (= [1 3 5 7 9]
        (sequence
         (comp
          ->hyperduction
          (hd-take-last 2)
          (hd-sum)
          <-hyperduction)
         [1 2 3 4 5]))))

  (defn hd-product [] (commutative-hyperducer * / 1))
  
  (testing "the superduction of the PRODUCT of a sliding window size 2..."
    (is
     (= [1 2 6 12 20]
        (sequence
         (comp
          ->hyperduction
          (hd-take-last 2)
          (hd-product)
          <-hyperduction)
         [1 2 3 4 5]))))

  (defn non-commutative-hyperducer [fa fd i z]
    (hyperducer
     (fn [acc [maybe-d maybe-a]]
       (let [new-acc (fa (fd acc (vor maybe-d z)) (vor maybe-a z))]
         [new-acc [[] [new-acc]]]))
     i))

  (defn unmerge [m1 m2]
    (reduce (fn [a [k v]] (if (= (a k) v) (dissoc a k))) m1 m2))

  (testing "unmerging one map from another"
    (is
     (= {:a 1}
        (unmerge {:a 1 :b 2 :c 3} {:b 2 :c 3}))))

  ;; thinking about merge/unmerge of a hashmap ?

  ;; thinking about hd-group-by and hd-frequencies...
  
  )

;; TODO:
;; batched hyperduction
;; investigate category theory, groups, symmetric inverse semigroups etc...
