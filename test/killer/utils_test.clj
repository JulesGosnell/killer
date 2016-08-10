(ns killer.utils-test
  (:require
   [killer.utils :refer :all]
   [clojure.test :refer :all]))

(deftest test-utils

  (testing "swap-first!"
    (let [a (atom 0)]
      (is (= 1 (swap! a inc)))
      (is (= "some other values" (swap-first! a (fn [i] [(inc i) "some other values"]))))
      (is (= 2 @a))))

  (testing "vassoc"
    (let [v1 [0 0 0]
          v2 (vassoc v1 1 1)
          v3 (vassoc v1 1 0)
          v4 (assoc v1 1 0)]
      (is (identical? v1 v1))
      (is (not (identical? v1 v2)))
      (is (identical? v1 v3))
      (is (not (identical? v1 v4)))
      ))

  (testing "anonymous multi-methods"
    (let [add-fn (make-multifn (fn [t & rest] (type t)))
          del-fn (make-multifn (fn [t & rest] (type t)))]
      (make-method! add-fn Long +)
      (make-method! add-fn String str)
      (make-method! del-fn Long -)
      (make-method! del-fn String (fn [& rest] (apply str (reverse rest))))
      (is (= (add-fn 0 1 2 3) 6))
      (is (= (add-fn "a" "b" "c" "d" "e") "abcde"))
      (is (= (del-fn 0 1 2 3) -6))
      (is (= (del-fn "a" "b" "c" "d" "e") "edcba"))
      ))
        
  (testing "mfn"
    (let [add-fn (mfn
                  (fn [t & rest] (type t))
                  [[Long +][String str]])]
      (is (= (add-fn 0 1 2 3) 6))
      (is (= (add-fn "a" "b" "c" "d" "e") "abcde"))
      ))

  (testing "count-by"
    (is (= (count-by even? (range 9)) 5))
    (is (= (count-by odd?  (range 9)) 4)))
  )
