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


  ;; let's go crazy and try writing a join hyperducer...'

  ;; we'll nee hswap!

  (defn swap-first! [a f & args]
    (loop []
      (let [old-val @a
            [new-val other] (apply f old-val args)]
        (if (compare-and-set! a old-val new-val)
          other
          (recur)))))

  (testing "swap-first!"
    (let [a (atom 0)]
      (is (= 1 (swap! a inc)))
      (is (= "some other values" (swap-first! a (fn [i] [(inc i) "some other values"]))))
      (is (= 2 @a))))


  ;; this is fairly complex stuff, so I am going to think in Scala so I
  ;; can use types (aargh - hypocrite !!)

  ;; def join[L,R,K](
  ;;     left: HyperChannel[L],
  ;;     leftKeyFn: (L)=>K,
  ;;     right: HyperChannel[R],
  ;;     rightKeyFn: (R)=>K,
  ;;     ):
  ;;     (
  ;;     joined: HyperChannel[(L, R)],
  ;;     unjoinedLeft: HyperChannel[L],
  ;;     unjoinedRight: HyperChannel[R],
  ;;     ) = {
  ;;     ...
  ;;     }
  

  (defn zip-with-index [& seqs] (apply mapv (fn [& rest] rest) (range) seqs))

  (testing "zip-with-index"
    (is (= [[0 :a :A][1 :b :B][2 :c :C]] (zip-with-index [:a :b :c] [:A :B :C]))))

  (defn vassoc [s i v]
    "associate a value into e.g. a vector but return original vector if result would be same by value"
    (if (= v (nth s i)) s (assoc s i v)))

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
  
  ;; WIP

  (def not-empty? (comp not empty?))

  (defn- join-hyperducer [state default-val index key-fn]
    (fn [xf]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [[output joined]    ;TODO: raise joined event to joined channel - somehow ?
               (swap-first!
                state
                (fn [s0 [maybe-d maybe-a]]
                  (let [[s1 o1 j1]
                        ;; (if (not-empty? maybe-d)
                        ;;   (let [[d] maybe-d
                        ;;         k [(key-fn d)] ;; lift key, so we an support nil keys
                        ;;         old-val (s0 k default-val)
                        ;;         new-val (assoc old-val index nil)
                        ;;         joined (every? (comp not empty?) old-val)]
                        ;;     [(if (every? empty? new-val) (dissoc s0 k) (assoc s0 k new-val)) ;new state
                        ;;      (if (nth old-val index) nil [d]) ;deletion event for output channel
                        ;;      (if (every? (comp not empty?) old-val) [old-val] nil)]) ;deletion event for joined channel
                        [s0 [nil nil] [nil nil]]
                        ;;   )
                        [s2 o2 j2]
                        (if (not-empty? maybe-a)
                          (let [[a] maybe-a
                                k [(key-fn a)] ;; lift key, so we an support nil keys
                                old-val (s1 k default-val)
                                new-val (vassoc old-val index maybe-a) ;; lift value, so we can support nil values
                                joined (every? (comp not empty?) new-val)]
                            [(assoc s1 k new-val)              ;new-state
                             (if (identical? old-val new-val) [nil nil] (if joined [[a] nil] [nil [a]]));addition event for output channel
                             (if joined [nil new-val] [nil nil]);addition event for joined channel
                             ])
                          [s1 [nil nil] [nil nil]])] 
                    ;; we may have painted ourselves into a corner by implying that a hyperducer is a stream of pairs of SINGLE events :-(
                    ;; should be (reduce foo state maybe-d) NOT (if maybe-d ...) - then we could handle event batches...
                    [s2            ;new state for atom
                     [o2    ;deletion and addition for output stream
                      j2]]))
                input)]; deletion and addition for join stream
           (xf result output)))))    
    )

  (testing "join-hyperducer - 3 insertions / 0 joins"
    (is
     (=
      (sequence
       (join-hyperducer
        (atom {})              ;empty state
        [nil nil]              ;we're joining 2 streams
        0                      ;we are the first stream
        identity               ;our key-fn
        )
       (sequence ->hyperduction  [1 2 3]))
      [[nil [1]] [nil [2]] [nil [3]]])))

  (testing "join-hyperducer - 3 insertions / 3 joins"
    (is
     (=
      (sequence
       (join-hyperducer
        (atom {[1] [[1] nil] [2] [[2] nil] [3] [[3] nil]}) ; we've already read in some state - joins awaiting...
        [nil nil]                       ; we are joining 2 streams
        1                               ; we are the second stream
        identity                        ; our key-fn
        ) (sequence ->hyperduction  [1 2 3]))
      [[[1] nil] [[2] nil] [[3] nil]])))
    

  ;; ;; NYI
  ;; (defn join-hyperducers [key-fns output-channel]
  ;;   (let [state (atom {})
  ;;         n (count keyfns)
  ;;         default-val (vec (repeat n nil))]
  ;;     (mapv
  ;;      (fn [index key-fn] (join-hyperducer state default-val index key-fn))
  ;;      (range)
  ;;      key-fns)))
  
  )

;; TODO:
;; batched hyperduction
;; investigate category theory, groups, symmetric inverse semigroups etc...

;;------------------------------------------------------------------------------

;; lets see if we can write a join() hyperducer - needed for tasks like reconciliations

;; - assumes strict ordering
;; - maintains a Map[key: K, (leftValue: Option[L], rightValue: Option[R])]
;; - when an item arrives from either right or left, we apply the relevant keyFn then look up the result in our map

;; - we need to perform a swap on an atom and return a tuple of [optional-event-for-unjoined-left-channel, optional-event-for-unjoined-right-channel, optional-event-for-joined-channel, new value for atom]



;; do we need to generate two of these functions ? one to work on the left channel and one to work on the right ?

;; If we can get this working we should be able to use it reconcile transfers and orders\
