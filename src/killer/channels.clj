(ns killer.channels
  (:require
   [clojure.core.async :as async]))

;;------------------------------------------------------------------------------
;; transducers operate on a single physical in/out stream
;; some killer transducers need to operate on multiple logical in and out streams

;; this can be achieved by multiplexing input streams together and
;; demultiplexing the output stream into many

(defn multiplex [mf channels]
  (async/merge
   (mapv
    (fn [i c] (async/map (fn [e] (mf i e)) [c]))
    (range)
    channels)))

(defn demultiplex [df i os]
  (async/go
    (loop []
      (if-let [e (async/<!! i)]
        (let [[i v] (df e)]
          (async/>! (nth os i) v)
          (recur))
        (doseq [o os]
          (async/close! o)))))
  os)
