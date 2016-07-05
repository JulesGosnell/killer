(ns killer.main
  (:require
   [killer
    [server :as server]
    [whales :as whales]
    [trades :as trades]])
  (:gen-class))
 
(defn -main []
  (server/add-models "Trades by currency and totalled value"     trades/headers trades/body (trades/start))
  (server/add-models "Whales by species and totalled sightings." whales/headers whales/body (whales/start))
  (server/start!))
