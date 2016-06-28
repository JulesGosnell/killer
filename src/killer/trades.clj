(ns killer.trades
  (:require
   [clojure.core.async :as async  :refer (<! <!! >! >!! chan go go-loop)]
   [hiccup.core        :as hiccup]))

(def body [:div {:data-chart-type "donut" :data-channel-name "trades"}])

(def currencies [:GBP :USD :EUR :JPY :CNY :AUD :NZD])
(def amounts [25 50 75 100])

(defn random-element [s] (nth s (rand-int (count s))))

(defn random-trade [] {:currency (random-element currencies)
                       :value (random-element amounts)})

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

(defn esp-take-last
  [n]
  (esp-reduce
   (fn [a v] (conj (if (= (count a) n) (pop a) a) v))
   (clojure.lang.PersistentQueue/EMPTY)))

(defn donut [s]
  [:killer/trades (mapv (fn [[k v]] {:label k :value v}) s)])

(defn start []
  (let [c (chan
         1024
         (comp
          (esp-take-last 90)          ;sliding windows of last 90 events
          (map (fn [s] (group-by :currency s)))
          (map (fn [s] (map (fn [[k vs]] [(name k) (reduce (fn [a {v :value}] (+ a v)) 0 vs)]) s)))
          (map (fn [s] (sort-by first s))) ;sort frequencies by currency
          (map donut) ;reformat for front-end
          )
         )]
  (future
    ;; generate new trades...
    (while true
      (do
        (>!! c (random-trade))
        (Thread/sleep 1000))))
  c))
