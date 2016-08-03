(ns killer.hyperducers
  (:require
   [killer.utils :refer :all]))

;;------------------------------------------------------------------------------
;; a hyperducer operates on a stream/sequence of the following types:

(defrecord Addition [value])
(defrecord Deletion [value])

;;------------------------------------------------------------------------------

;; these will lift and lower streams of insertion ONLY into/out-of
;; hyperduction streams

(def ->hyperduction (map (fn [e] (->Addition e))))
(def <-hyperduction (map :value))

;;------------------------------------------------------------------------------
;; take-last

;; interestingly - takes a transduction and converts it into a
;; hyperduction repesenting a fixed size sliding window across the
;; transduction...

(defn- stateful-transducer
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

(defn take-last-hyperducer
  [n]
  (stateful-transducer
   (fn [old-a {v :value}]
     (let [[new-a deletion]
           (if (= (count old-a) n)
             [(pop old-a) [(first old-a)]]
             [old-a []])]
       [(conj new-a v)
        (conj (mapv ->Deletion deletion) (->Addition v))]))
   (clojure.lang.PersistentQueue/EMPTY)))


;; hmmm...
;; should take-last expect a hyperduction or transduction stream?

;; at the moment it expects a hyperduction stream of Additions ONLY - can't be right

;;------------------------------------------------------------------------------
;; join hyperducer

;; returns [new-state events]
(defmulti join (fn [state index join-index multiplex-function key-function default-val event] (type event)))

(defmethod join Deletion [state index join-index multiplex-function key-function default-val {deletion :value}]
  (let [key (key-function deletion)
        old-val (or (state key) default-val)
        new-val (assoc old-val index nil)]
    [
     (if (every? empty? new-val)
       ;; this join is now completely empty - remove it from state
       (dissoc state key)
       ;; update state with new version of join
       (assoc state key new-val))

     (if (every? (comp not empty?) old-val)
       ;; we are breaking an existing join...
       (conj
        ;; issue addition events for all channels except our own
        (mapv
         (fn [[i [v]]]
           (multiplex-function i (->Addition v)))
         (filter
          (fn [[i maybe-v]] (not (empty? maybe-v)))
          (mapv vector (range) new-val)))
        ;; issue deletion on join channel
        (multiplex-function join-index (->Deletion (flatten old-val)))
        )
       ;; we are not breaking an existing join
       [
        ;; issue a deletion event on our own channel
        (multiplex-function index (->Deletion deletion))
        ])
     ]))
  
(defmethod join Addition [state index join-index multiplex-function key-function default-val {addition :value}]
  (let [key (key-function addition)
        old-val (or (state key) default-val)
        new-val (assoc old-val index [addition])]
    [
     (assoc state key new-val)
     (if (every? (comp not empty?) new-val)
       ;; we are creating a new join
       (conj
        ;; issue deletion events for all channels for which we have a corresponding value
        (mapv
         (fn [[i [v]]]
           (multiplex-function i (->Deletion v)))
         (filter
          (fn [[i maybe-v]] (not (empty? maybe-v)))
          (mapv vector (range) old-val)))
        ;; issue addition on join-channel
        (multiplex-function join-index (->Addition (flatten new-val)))
        )
       ;; we aew not creating a new join
       [
        ;; issue an addition event on our own channel
        (multiplex-function index (->Addition addition))
        ])
     ]))
  
(defn join-hyperducer [mf df keyfns]
  (let [state (atom {})
        n (count keyfns)
        default-val (vec (repeat n nil))]
    (fn [xf]
      (fn
        ([]
         (xf))
        ([result]
         (xf result))
        ([result input]
         (let [[i v] (df input)]
           (xf result (swap-first! state join i n mf (nth keyfns i) default-val v))))
        ))))

;;------------------------------------------------------------------------------
;; what about :
;; - versions
;; - metadata
;; - union
;; - etc...
;; ??

;; thinking about merge/unmerge of a hashmap ?

;; thinking about hd-group-by and hd-frequencies...

;; TODO:
;; batched hyperduction
;; investigate category theory, groups, symmetric inverse semigroups etc...

