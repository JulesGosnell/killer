(ns killer.test-utils
  (:require
   [clojure.core.async :as async]))

(defn to-vec [c]
  (let [v (atom [])]
    (loop []
      (when-let [e (async/<!! c)]
        (do
          (swap! v conj e)
          (recur))))
    @v))


