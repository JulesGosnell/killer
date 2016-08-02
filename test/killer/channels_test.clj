(ns killer.channels-test
  (:require
   [clojure.test :refer :all]
   [clojure.core.async :as async]
   [killer.channels :refer :all]
   [killer.test-utils :refer :all]
   ))

(deftest test-channels

  (testing "multiplex"
    
    (let [input0 (async/to-chan [:a :b :c])
          input1 (async/to-chan [:A :B :C])
          output (multiplex vector [input0 input1])]
      (is
       (=
        (sort (to-vec output))            ;N.B. ordering is unknown...
        [[0 :a] [0 :b] [0 :c] [1 :A] [1 :B] [1 :C]]))
      )

    )

  
  (testing "demultiplex"
    
    (let [input (async/to-chan [[0 :a] [1 :A] [0 :b] [1 :B] [0 :c] [1 :C]])
          [output1 output2] (demultiplex identity input [(async/chan 1024)(async/chan 1024)]) 
          ]
      (is
       (=
        (to-vec output1)
        [:a :b :c]))
      (is
       (=
        (to-vec output2)
        [:A :B :C]))
      )
    )
  )
