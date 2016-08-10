(ns killer.hyperducers
  (:require
   [clojure.data :refer :all]
   [flatland.ordered.map :refer :all]
   [killer.utils :refer :all]))

;;------------------------------------------------------------------------------
;; a hyperducer operates on a stream/sequence of sequences of the following types:

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
;; poll-hyperducer

;; input: sequence resulting from e.g. polling a resource
;; output; a hyperduction describing the difference between this sequence and the last one
  
(defn- hdiff [before after]
  [after
   (concat
    (mapv (comp ->Deletion second) (filter (fn [[k]] (not (contains? after k))) before))
    (mapv (comp ->Addition second) (filter (fn [[k]] (not (contains? before k))) after)))])


(defn poll-hyperducer [key-fn]
  (let [state (atom (ordered-map))]
    (fn [xf]
      (fn
        ([]
         (xf))
        ([result]
         (xf result))
        ([result input]
         (xf
          result
          (swap-first! state hdiff (index-by ordered-map key-fn input))))))))

;;------------------------------------------------------------------------------
;; join hyperducer

;; returns [new-state events]

;; hmmm - now supports partial joins but:
;; what happens if we update an element invoved in a join ?
;; what happens we form a partial join and then an element on a stream not yet involved in the join has a matching key ?
(defn join-hyperducer [mf df keyfns]
  (let [state (atom {})
        n (count keyfns)
        joins-needed n
        default-val (vec (repeat n nil))
        join
        (mfn
         (fn [state index event] (type event))
         [[Deletion
           (fn [state index {deletion :value}]
             (let [key ((nth keyfns index) deletion)
                   old-val (or (state key) default-val)
                   new-val (vassoc old-val index nil)]
               [
                (if (every? empty? new-val)
                  ;; this join is now completely empty - remove it from state
                  (dissoc state key)
                  ;; update state with new version of join
                  (assoc state key new-val))

                (if (and
                     (= (count-by (comp not empty?) old-val) joins-needed)
                     (not (identical? old-val new-val)))
                  ;; we are breaking an existing join...
                  (conj
                   ;; issue addition events for all channels involved
                   (mapv
                    (fn [[i [v]]]
                      (mf i (->Addition v)))
                    (filter
                     (fn [[i maybe-v]] (not (empty? maybe-v)))
                     (mapv vector (range) new-val)))
                   ;; issue deletion on join channel
                   (mf n (->Deletion (flatten old-val)))
                   )
                  ;; we are not breaking an existing join
                  [
                   ;; issue a deletion event on our own channel
                   (mf index (->Deletion deletion))
                   ])
                ]))]
          [Addition
           (fn [state index {addition :value}]
             (let [key ((nth keyfns index) addition)
                   old-val (or (state key) default-val)
                   new-val (vassoc old-val index [addition])]
               [
                (assoc state key new-val)
                (if (and
                     (= joins-needed (count-by (comp not empty?) new-val))
                     (not (identical? old-val new-val)))
                  ;; we are creating a new join
                  (conj
                   ;; issue deletion events for all channels for which we have a corresponding value
                   (mapv
                    (fn [[i [v]]]
                      (mf i (->Deletion v)))
                    (filter
                     (fn [[i maybe-v]] (not (empty? maybe-v)))
                     (mapv vector (range) old-val)))
                   ;; issue addition on join-channel
                   (mf n (->Addition (flatten new-val)))
                   )
                  ;; we are not creating a new join
                  [
                   ;; issue an addition event on our own channel
                   (mf index (->Addition addition))
                   ])
                ]))]])]
    (fn [xf]
      (fn
        ([]
         (xf))
        ([result]
         (xf result))
        ([result input]
         (let [[i v] (df input)]
           (xf result (swap-first! state join i v))))
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

