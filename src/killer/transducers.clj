(ns killer.transducers)

;;------------------------------------------------------------------------------
;; private infrastructure

(defn- esp-stateless-reduce
  "running reduction of values seen"
  [f i]
  (fn [xf]
    (fn
      ([] (xf nil))
      ([_] (xf nil))
      ([result input]
       (xf result (f input))))))

(defn- esp-reduce
  "running reduction of values seen"
  [f i]
  (fn [xf]
    (let [accumulator (volatile! i)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (vswap! accumulator (fn [a] (f a input)))
         (xf result @accumulator))))))

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
  (esp-reduce conj []))

(defn esp-window
  "running fixed size sequence of values seen"
  [n]
  ;; TODO: use a better suited and therefore more efficient data-structure - maybe mutable - investigate
  (esp-reduce (fn [a v] (conj (if (= (count a) n) (vec (rest a)) a) v)) [])
  ;; needs to use a stack...
  ;;(esp-reduce (fn [a v] (conj (if (= (count a) n) (pop a) a) v)) [])
  )

(defn esp-hash-set
  "running hash-set of values seen"
  []
  (esp-conj #{}))

(defn esp-hash-map
  "running hash-map of values seen"
  [key-fn]
  (esp-reduce (fn [a v] (assoc a (key-fn v) v)) {}))

;;------------------------------------------------------------------------------
;; sequences -> simple values
;;------------------------------------------------------------------------------
;; sequences
;; sequence operations

;; this 'last' expects each element of input to be a seq
;; an alternate 'last' would have aggregate all inputs seen and taken the last one - not much point
(defn esp-last
  "running last of latest sequence seen"
  []
  (esp-stateless-reduce last nil))

;;------------------------------------------------------------------------------
;; sequences -> sequences
;;------------------------------------------------------------------------------
;; sequences -  transformations

(defn esp-sort
  "running sort of latest sequence seen"
  []
  (esp-stateless-reduce sort nil))
  
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

(defn- pie-chart [frequencies divisor]
  "transform a hash-map of value:frequency to value:proportion using given divisor"
  (let [n (apply + (vals frequencies))]
    (map (fn [[k v]] [k (* (/ v n) divisor)]) frequencies)))

(defn esp-pie-chart
  "live pie-charting of frequency of values seen"
  [n]
  (esp-stateless-reduce (fn [v] (into {} (pie-chart v n)))[]))

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
   (esp-last)))

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
   (esp-sort)
   (esp-stateless-reduce median nil)))


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
   (esp-stateless-reduce mode nil)
   )
  )
 
;;------------------------------------------------------------------------------

;; versioning could be a single transducer ?

