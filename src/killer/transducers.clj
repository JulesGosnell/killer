(ns killer.transducers)


;;------------------------------------------------------------------------------
;; private infrastructure

(defn- esp-reduce
  "a stateful transducer producing a running reduction of values seen"
  [f i]
  (fn [xf]
    (let [accumulator (volatile! i)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (vswap! accumulator (fn [a] (f a input)))
         (xf result @accumulator))))))

(defn esp-apply
  "running application of a function on values seen"
  [f]
  (map (fn [s] (apply f s))))

;;------------------------------------------------------------------------------
;; simple values -> simple values
;;------------------------------------------------------------------------------
;; maths - simple

(defn esp-+
  "running sum of values seen"
  []
  (esp-reduce + 0))

(defn esp-*
  "running product of values seen"
  []
  (esp-reduce * 1))

;;------------------------------------------------------------------------------
;; simple values -> sequences
;;------------------------------------------------------------------------------
;; sequences - building

(defn esp-conj
  "running conjunction of values seen"
  [i]
  (esp-reduce conj i))

(defn esp-vector
  "running vector of values seen"
  []
  (esp-conj []))

;; N.B.
;; esp-take-last is equivalent to the ESP idea of a "sliding window"
;; of the last n values...

;; naively:

;; (defn esp-take-last
;;   [n]
;;   (comp
;;    (esp-vector)
;;    (map (fn [s] (take-last n s)))))

;; more efficiently:

(defn esp-take-last
  [n]
  (esp-reduce
   (fn [a v] (conj (if (= (count a) n) (pop a) a) v))
   (clojure.lang.PersistentQueue/EMPTY)))

;; hmmm... introducing a window raises a whole host of issues...
;; namely...

;; it means that values can leave aswell as join a stream, so we need
;; some way of representing deletion

;; it means that downstream transducers should now expect a stream of
;; vectors of values instead of a stream of values...
;; re-evaluating an entire vector each time means expensive processing will be repeated over and over again
;; so we need to move to a delta-ed model, but...

;; aargh !!

(defn esp-hash-set
  "running hash-set of values seen"
  []
  (esp-conj #{}))

;; N.B.
;; clojure.core/hash-map expects to be applied to a plist - this
;; doesn't make much sense in an ESP context so esp-hash-map expects a
;; stream of [k v] pairs.
(defn esp-hash-map
  "running hash-map of [k v]'s seen"
  []
  (esp-conj {}))

;; N.B.
;; hash-map-by doesn't exist in clojure.core, but I use the equivalent
;; all the time in ESP - so let's pretend that it does...

;; naively

;; (defn esp-hash-map-by
;;   "running hash-map of values seen"
;;   [key-fn]
;;   (comp
;;    (map (fn [v] [(key-fn v) v]))
;;    (esp-conj {}))
;;   )

;; more efficiently

;; avoid intermdiate tuples
(defn esp-hash-map-by
  "running hash-map of values seen"
  [key-fn]
  (esp-reduce (fn [a v] (assoc a (key-fn v) v)) {})
  )

;; SIMPLIFIED TO HERE

;;------------------------------------------------------------------------------
;; sequences -> simple values
;;------------------------------------------------------------------------------
;; sequences
;; sequence operations

;;------------------------------------------------------------------------------
;; sequences -> sequences
;;------------------------------------------------------------------------------
;; sequences -  transformations

;; TODO: we should really be looking at a window/take-last, so does
;; this make sense anymore ?
(defn esp-group-by
  "running group-by of values seen"
  [key-fn f init]
  (esp-reduce
   (fn [a v]
     (let [k (key-fn v)
           vs (or (a k) init)]
       (assoc a k (f vs v))))
   {}))

(defn esp-frequencies
  "running table of frequencies of values seen"
  []
  (esp-group-by identity (fn [vs v] (inc vs)) 0))

(defn- pie-chart
  "transform a hash-map of value:frequency to value:proportion of a given whole"
  [frequencies whole]
  (let [n (apply + (vals frequencies))]
    ;; TODO: we should be able to avoid an intermediate sequence here by using transducers ?
    (into {} (map (fn [[k v]] [k (* (/ v n) whole)]) frequencies))))

(defn esp-pie-chart
  "live pie-charting of frequency of values seen"
  [whole]
  (map (fn [v] (pie-chart v whole))))

;;------------------------------------------------------------------------------
;; simple values -> simple values
;;------------------------------------------------------------------------------
;; maths - more complex

(defn esp-mean
  "running mean of values seen"
  ;; accumulate tuple of [sum size avg] then take last elt
  []
  (comp
   (esp-reduce
    (fn [[size sum] v]
      (let [new-size (inc size)
            new-sum (+ sum v)]
        [new-size new-sum  (/ new-sum new-size)]))
    [0 0 0])
   (map last)))

(defn- median [s]
  "return the median of a ordered sequence of numbers"
  (let [n (count s)]
    (when (not (zero? n))
      (let [p (/ n 2)]
        (if (integer? p)
          (/ (+ (nth s (dec p)) (nth s p)) 2)
          (nth s p))))))

(defn esp-median
  "running median of values seen"
  []
  (comp
   ;; TODO: what we really want here is a sorted bag of some sort - investigate
   ;; maybe even a mutable java collection...
   (esp-vector)
   (map sort)
   (map median)))


(defn- mode [frequencies]
  "given a map of value:frequency, return the single value with the highest frequency or nil"
  (let [m (apply max (vals frequencies))
        pairs (filter (fn [[k v]] (= v m)) frequencies)]
    (when (= (count pairs) 1)
      (first (first pairs)))))

;; mode
;; naive impl
;; TODO: remember mode from last time and filter for higher ?
;; TODO: could do better by just doing all in one...
(defn esp-mode
    "running mode of values seen"
  []
  (comp
   (esp-frequencies)
   (map mode)
   )
  )
 
;;------------------------------------------------------------------------------

;; versioning could be a single transducer ?

;;------------------------------------------------------------------------------

;; THOUGHTS:

;; often passing by value is too expensive and we want a stream of deltas...
;; but not always...
;; what we want is a stream that offers both possibilities but that would be all the expense of by value plus by-delta...

;; what if we had a stream of [value-fn, delta-fn] ? i.e. the values were realised lazily...

;; each superducer would receive a pair of fns and choose which one to call...

;; e.g.

;; we have window size 3
;; last window was [1 2 3]
;; we put a 4 on the stream
;; we have (+) superducer on the stream
;; it receives [-> [2 3 4], -> [delete: 1, upsert: 4]
;; it can choose to take by value and sum up the whole value again
;; it can choose to take by delta, substract 1 and add 4
;; it will produce [-> 9, -> [delete: 6, upsert: 9]]

;; downside - since work is done lazily, it will all happen on a single processor... maybe not true

;; the first time a superducer receives an event it can call the by-value fn
;; subsequently it can call the delta-fn
;; should we make it a triple where the firt function returns meta data ?

;; lets put deltas to one side for a while and focus on producing
;; something that works, even if not very performant

;;==============================================================================
;; lets try some tranducers driven by deltas
;;==============================================================================

(defrecord Upsertion [value])
(defrecord Deletion [value])

(defn- dd-reduce
  [f i]
  (fn [xf]
    (let [accumulator (volatile! i)]
      (fn
        ([] (second (xf)))
        ([result] (second (xf result)))
        ([result input]
         (vswap! accumulator (fn [a] (f a [(Upsertion. input)])))
         (xf result (second @accumulator)))))))

(defn dd-window
  [n]
  (esp-reduce
   (fn [[a _] v]
     (if (= (count a) n)
       (let [d (first a)]
         [(conj (pop a) v)
          [(Deletion. d)(Upsertion. v)]])
       [(conj a v)
        [(->Upsertion v)]]))
   [(clojure.lang.PersistentQueue/EMPTY) nil]))
