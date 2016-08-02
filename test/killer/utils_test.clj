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

  )
