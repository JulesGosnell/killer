(ns killer.core
  (require [clojure.core.async :as async :refer [<! >! <!! >!! timeout chan alt! alts!! go]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
) 
