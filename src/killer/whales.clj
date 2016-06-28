(ns killer.whales
  (:require
   [clojure.core.async :as async  :refer (<! <!! >! >!! chan go go-loop)]
   [hiccup.core        :as hiccup]))

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
  [:killer/whales (mapv (fn [[k v]] {:label k :value v}) s)])

(defn start []
  (let [c (chan
         1024
         (comp
          (esp-take-last 90)          ;sliding windows of last 90 events
          (map frequencies)           ;aggregate whale frequencies
          (map (fn [s] (sort-by first s))) ;sort frequencies by whale type
          (map donut) ;reformat for front-end
          )
         )]
  (future
    ;; generate new whale sightings...
    (while true
      (do
        (>!! c (random-element whales))
        (Thread/sleep 1000))))
  c))
