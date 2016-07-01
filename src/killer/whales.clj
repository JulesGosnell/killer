(ns killer.whales
  (:require
   [clojure.core.async :as async  :refer (<! <!! >! >!! chan go go-loop)]
   [hiccup.core        :as hiccup]
   [killer.transducers :refer :all]))

(def body [:div {:data-chart-type "donut" :data-channel-name "whales"}])

(def whales
  [
   "Blue Whale"
   "Right Whale"
   "Humpback Whale"
   "Grey Whale"
   "Minke Whale"
   "Beluga Whale"
   "Narwhal"
   "Killer Whale"
   "Sperm Whale"
   "Pygmy Sperm Whale"
   "Pilot Whale"
   "Bottlenose Dolphin"
   "Harbour Porpoise"
   "Amazon River Dolphin"
   "Spinner Dolphin"
   "Dusky Dolphin"
   "False Killer Whale"
   "Yangtse River Dolphin"
   ])

(defn random-element [s] (nth s (rand-int (count s))))

(defn donut [s]
  [:killer/whales (mapv (fn [[k v]] {:label k :value v}) s)])

(defn start []
  (let [c (chan
         1024
         (comp
          (esp-take-last 90)       ;sliding window of last 90 events
          (map frequencies)          ;aggregate whale frequencies
          (map (fn [s] (sort-by first s))) ;sort frequencies by whale type
          (map donut)                      ;reformat for front-end
          )
         )]
  (future
    ;; generate new whale sightings...
    (while true
      (do
        (>!! c (random-element whales))
        (Thread/sleep 1000))))
  [c]))
