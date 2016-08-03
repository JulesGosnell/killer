(ns killer.hyperducers-test
  (:require
   [clojure.test :refer :all]
   [killer.hyperducers :refer :all]
   [killer.test-utils :refer :all]))

;; (deftest test-hyperduction

;;   ;; deprecated and being rewritten...
  
;;   ;; we can implement a sliding window - with values both entering one end and leaving at the other - as a hyper/transducer

;;   (defn- superducer
;;     [f i]
;;     (fn [xf]
;;       (let [accumulator (volatile! i)]
;;         (fn
;;           ([] (xf))
;;           ([result] (xf result))
;;           ([result input] (xf result (vswap! accumulator f input)))))))
  
;;   (testing "superduction"
;;     (is
;;      (= [1 3 6]
;;         (sequence (superducer + 0) [1 2 3]))))


;;   (defn vor [a b] (if (empty? a) b (first a)))
  
;;   (defn commutative-hyperducer [fa fd i]
;;     (hyperducer
;;      (fn [acc [maybe-d maybe-a]]
;;        (let [new-acc (fa (fd acc (vor maybe-d i)) (vor maybe-a i))]
;;          [new-acc [[] [new-acc]]]))
;;      i))
  
;;   (testing "commutative hyperduction"
;;     (is
;;      (= [[[][1]] [[][3]] [[][5]] [[][7]] [[][9]]]
;;         (sequence
;;          (commutative-hyperducer + - 0)
;;          [[[][1]] [[][2]] [[1][3]] [[2][4]] [[3][5]]]))))

;;   (defn hd-sum [] (commutative-hyperducer + - 0))
  
;;   (testing "the superduction of the SUM of a sliding window size 2..."
;;     (is
;;      (= [1 3 5 7 9]
;;         (sequence
;;          (comp
;;           ->hyperduction
;;           (hd-take-last 2)
;;           (hd-sum)
;;           <-hyperduction)
;;          [1 2 3 4 5]))))

;;   (defn hd-product [] (commutative-hyperducer * / 1))
  
;;   (testing "the superduction of the PRODUCT of a sliding window size 2..."
;;     (is
;;      (= [1 2 6 12 20]
;;         (sequence
;;          (comp
;;           ->hyperduction
;;           (hd-take-last 2)
;;           (hd-product)
;;           <-hyperduction)
;;          [1 2 3 4 5]))))

;;   (defn non-commutative-hyperducer [fa fd i z]
;;     (hyperducer
;;      (fn [acc [maybe-d maybe-a]]
;;        (let [new-acc (fa (fd acc (vor maybe-d z)) (vor maybe-a z))]
;;          [new-acc [[] [new-acc]]]))
;;      i))

;;   (defn unmerge [m1 m2]
;;     (reduce (fn [a [k v]] (if (= (a k) v) (dissoc a k))) m1 m2))

;;   (testing "unmerging one map from another"
;;     (is
;;      (= {:a 1}
;;         (unmerge {:a 1 :b 2 :c 3} {:b 2 :c 3}))))

;;   )

(deftest test-hyperducers

  (testing "->hyperduction"
    (is
     (=
      [(->Addition nil)(->Addition 1)(->Addition 2)(->Addition 3)]
      (sequence ->hyperduction [nil 1 2 3]))))

  (testing "<-hyperduction"
    (is
     (= [nil 1 2 3]
        (sequence <-hyperduction [(->Addition nil)(->Addition 1)(->Addition 2)(->Addition 3)]))))

  (testing "round-trip hyperduction"
    (is
     (= [nil 1 2 3]
        (sequence (comp ->hyperduction <-hyperduction) [nil 1 2 3]))))

  (testing "take-last-hyperducer"
    (is
     (=
      [[(->Addition nil)]
       [(->Addition 1)]
       [(->Deletion nil) (->Addition 2)]
       [(->Deletion 1)(->Addition 3)]]
      (sequence (comp ->hyperduction (take-last-hyperducer 2)) [nil 1 2 3])
      )
     ))
  
  (testing "join-hyperducer"
    (is
     (=
      [
       [[0 (->Addition :a)]]

       [[1 (->Addition :a)]]

       [[0 (->Deletion :a)]
        [1 (->Deletion :a)]
        [3 (->Addition [:a :a :a])]]

       [[1 (->Addition :a)]
        [2 (->Addition :a)]
        [3 (->Deletion [:a :a :a])]]

       [[1 (->Deletion :a)]]

       [[2 (->Deletion :a)]]
       ]
      (sequence
       (join-hyperducer vector identity [identity identity identity])
       [
        [0 (->Addition :a)] ;; addition on left
        [1 (->Addition :a)] ;; addition in middle
        [2 (->Addition :a)] ;; addition on right
        [0 (->Deletion :a)] ;; deletion on left
        [1 (->Deletion :a)] ;; deletion in middle
        [2 (->Deletion :a)] ;; deletion on right
        ]
       )
      )))

  )
