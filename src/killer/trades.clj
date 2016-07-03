(ns killer.trades
  (:require
   [clojure.core.async :as async  :refer (<! <!! >! >!! chan go go-loop mult tap)]
   [hiccup.core        :as hiccup]
   [killer.transducers :refer :all]))

(def body
  [:div
   [:div {:data-chart-type "donut" :data-channel-name "trades-donut"}]

   [:table {:class="display" :cellspacing="0" :width="100%" :data-chart-type "datatable" :data-channel-name "trades-datatable"}
    
    [:thead
     [:tr
      [:th "Currency"]
      [:th "Total"]
      ]
     ]
    [:tfoot
     [:tr
      [:th "Currency"]
      [:th "Total"]
      ]
     ]
    ]
   ]
  )

(def currencies [:GBP :USD :EUR :JPY :CNY :AUD :NZD])
(def amounts [25 50 75 100])

(defn random-trade [] {:currency (rand-nth currencies)
                       :value (rand-nth amounts)})

;; transform data to fit donut chart
(defn donut [s]
  [:killer/trades-donut (mapv (fn [[k v]] {:label k :value v}) s)])

;; transform data to fit datatable
(defn datatable [s]
  [:killer/trades-datatable (mapv (fn [[k v]] [k v]) s)])

(defn start []
  (let [c (chan
           1024
           (comp
            (esp-take-last 90)       ;sliding window of last 90 events
            (map (fn [s] (group-by :currency s)))
            (map (fn [s] (map (fn [[k vs]] [(name k) (reduce (fn [a {v :value}] (+ a v)) 0 vs)]) s))) ;total up each group of trades
            (map (fn [s] (sort-by first s))) ;sort totals by currency
            ))
        m (mult c)
        dc (tap m (chan 1024 (map donut)))
        tc (tap m (chan 1024 (map datatable)))]
    (future
      ;; generate new trades...
      (while true
        (do
          (>!! c (random-trade))
          (Thread/sleep 1000))))
    [dc tc]))
