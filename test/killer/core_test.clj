(ns killer.core-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer :all]
            ))

;;------------------------------------------------------------------------------
;; utils

(defn kreduce [f i p]
  "synchronously, reduce the contents of a finite channel"
  (let [result (atom nil)
        latch (java.util.concurrent.CountDownLatch. 1)]
    (async/go
      (loop [acc i]
        (if-let [v (async/<! p)]
          (recur (f acc v))
          (do
            (reset! result acc)
            (.countDown latch)))))
    (.await latch)
    @result))

(defn to-seq [p]
  "synchronously, move the contents of a finite channel into a vector"
  (kreduce conj [] p))

;;------------------------------------------------------------------------------

(deftest test-to-seq

  (testing "synchronously, read a channel into a seq"
    (is
     (let [expected (range 10)
           actual (to-seq (async/to-chan expected))]
       (= actual expected))))

  (testing "pipeline"
    (is
     (let [expected (range 10)
           src (async/to-chan expected)
           tgt (async/chan)
           _ (async/pipeline 1 tgt (map identity) src)
           actual (to-seq tgt)]
       (= actual expected))))

  )
