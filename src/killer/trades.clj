(ns killer.trades
  (:require
   [clojure.core.async :as async  :refer (<! <!! >! >!! chan go go-loop)]
   [hiccup.core        :as hiccup]
   [killer.transducers :refer :all]))

(def body
  [:div
   [:div {:data-chart-type "donut" :data-channel-name "trades"}]

   [:table {:class="display" :cellspacing="0" :width="100%" :data-chart-type "datatable" :data-channel-name "trades-table"}
    
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

(defn random-element [s] (nth s (rand-int (count s))))

(defn random-trade [] {:currency (random-element currencies)
                       :value (random-element amounts)})

(defn donut [s]
  [:killer/trades (mapv (fn [[k v]]
                          {:label k :value v}
                          ;;[k v]
                          ) s)])

(defn start []
  (let [c (chan
         1024
         (comp
          (esp-take-last 90)       ;sliding window of last 90 events
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
  [c]))
