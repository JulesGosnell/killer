(ns killer.utils)

(defn swap-first! [a f & args]
  (loop []
    (let [old-val @a
          [new-val other] (apply f old-val args)]
      (if (compare-and-set! a old-val new-val)
        other
        (recur)))))

