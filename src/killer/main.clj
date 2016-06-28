(ns killer.main
  (:require
   [killer
    [server :as server]
    [whales :as whales]
    [trades :as trades]]))
 
(defn -main []
  (server/add-model "Whales by species and totalled sightings." whales/body (whales/start))
  (server/add-model "Trades by currency and totalled value" trades/body (trades/start))
  (server/start!))

