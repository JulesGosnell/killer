(ns killer.utils)

(defn swap-first! [a f & args]
  (loop []
    (let [old-val @a
          [new-val other] (apply f old-val args)]
      (if (compare-and-set! a old-val new-val)
        other
        (recur)))))

(defn vassoc [s i v]
  "associate a value into e.g. a vector but return original vector if result would be same by value"
  (if (= v (nth s i)) s (assoc s i v)))

(defn index-by [mf kf s]
  (apply mf (mapcat (fn [v] [(kf v) v]) s)))
