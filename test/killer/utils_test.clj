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

  )
