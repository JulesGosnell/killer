(ns killer.main
  (:require
   [killer
    [server :as server]
    [whales :as whales]
    [trades :as trades]]))
 
(defn -main []
  (server/add-model "Whales" whales/body (whales/start))
  (server/add-model "Trades" trades/body (trades/start))
  (server/start!))

