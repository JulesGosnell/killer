(ns killer.donut
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :as async  :refer (<! >! put! chan)]
   [d3 :as d3]
   ))

;; thanks to David Buezas for the JavaScript that I translated into
;; the original version of this program...
;; http://bl.ocks.org/dbuezas/9306799

(defn- key-fn [d] (.-label (.-data d)))

(defn- mid-angle [d]
  (+ (.-startAngle d) (/ (- (.-endAngle d) (.-startAngle d)) 2)))

(defn- get-state [node] (.-_current node))

(defn- set-state! [node s] (set! (.-_current node) s))

(defn- merge-with-first-equal-zero [first second]
  (let [second-set (let [tmp (d3/set)] (.forEach second (fn [d] (.add tmp (.-label d)))) tmp)
        only-first (.map
                    (.filter first (fn [d] (not (.has second-set (.-label d)))))
                    (fn [d] (js-obj "label" (.-label d) "value" 0)))]
    (.sort
     (d3/merge (array second only-first))
     (fn [a b] (d3/ascending (.-label a) (.-label b))))))

(defn render [root duration data-chan]

  (let [width 960
        height 450
        radius (/ (min width height) 2)
        pie (.value (.sort (.pie d3/layout) nil) (fn [d] (.-value d)))
        arc (.innerRadius (.outerRadius (.arc d3/svg) (* radius 0.8)) (* radius 0.4))
        outer-arc (.outerRadius (.innerRadius (.arc d3/svg) (* radius 0.9)) (* radius 0.9))

        svg (let [svg (.append (.append (d3/select root) "svg") "g")]
              (.attr (.append svg "g") "class" "slices")
              (.attr (.append svg "g") "class" "labels")
              (.attr (.append svg "g") "class" "lines")
              (.attr svg "transform" (str "translate(" (/ width 2) "," (/ height 2) ")"))
              svg)

        ;; colours:

        ;; first two elements of vector used to provide equivalent fn-ality
        ;; to a hash-set that preserves insertion order.
        ;; last element is a d3/color that is rebult each time an unseen k is encountered...

        ;; TODO:
        ;; this approach does not support the deallocation of colours -
        ;; ultimately we will run out...
        color (let [seen (atom [(hash-set) [] (.domain (.category20 d3/scale) (array))])]
                (fn [k]
                  ((nth
                    (swap!
                     seen
                     (fn [[old-s old-v old-c :as old] l]
                       (let [new-s (conj old-s l)]
                         (if (= old-s new-s)
                           old
                           (let [new-v (conj old-v l)
                                 new-c (.domain (.category20 d3/scale) (into-array new-v))]
                             [new-s new-v new-c])))))
                    2)
                   k)))
        
        change (fn [data]
                 (let [data0 (let [tmp (.map
                                        (.data (.selectAll (.select svg ".slices") "path.slice"))
                                        (fn [d] (.-data d)))]
                               (if (== (.-length tmp) 0) data tmp))
                       was (merge-with-first-equal-zero data data0)
                       is  (merge-with-first-equal-zero data0 data)

                       dss (fn [s1 s2 d]
                             (.data (.selectAll (.select svg s1) s2) (pie d) key-fn))

                       dtedss (fn [s1 s2]
                                (.delay (.transition (.exit (dss s1 s2 data))) duration))
                       ]

                   ;; slice arcs

                   (.each
                    (.style
                     (.attr
                      (.insert
                       (.enter
                        (dss ".slices" "path.slice" was))
                       "path")
                      "class"
                      "slice")
                     "fill"
                     (fn [d] (color (.-label (.-data d)))))
                    (fn [d] (set-state! (js* "this") d)))

                   (.attrTween
                    (.duration
                     (.transition
                      (dss ".slices" "path.slice" is))
                     duration)
                    "d"
                    (fn [d]
                      (let [this (js* "this")
                            interpolate (d3/interpolate (get-state this) d)]
                        (fn [t]
                          (let [state (interpolate t)]
                            (set-state! this state)
                            (arc state))))))
                   
                   (.remove (.duration (dtedss ".slices" "path.slice") 0))

                   ;; text labels

                   (.each
                    (.text
                     (.style
                      (.attr
                       (.append
                        (.enter
                         (dss ".labels" "text" was))
                        "text")
                       "dy"
                       ".35em")
                      "opacity"
                      0)
                     (fn [d] (.-label (.-data d))))
                    (fn [d] (set-state! (js* "this") d)))

                   (.styleTween
                    (.attrTween
                     (.style
                      (.duration
                       (.transition
                        (dss ".labels" "text" is))
                       duration)
                      "opacity"
                      (fn [d] (if (== (.-value (.-data d)) 0) 0 1)))
                     "transform"
                     (fn [d]
                       (let [this (js* "this")
                             interpolate (d3/interpolate (get-state this) d)]
                         (fn [t]
                           (let [d2 (interpolate t)]
                             (set-state! this d2)
                             (let [pos (.centroid outer-arc d2)]
                               (aset pos 0 (* radius (if (< (mid-angle d2) Math/PI) 1 -1)))
                               (str "translate(" pos ")")))))))
                    "text-anchor"
                    (fn [d]
                      (let [interpolate (d3/interpolate (get-state (js* "this")) d)]
                        (fn [t]
                          (let [d2 (interpolate t)]
                            (if (< (mid-angle d2) Math/PI) "start" "end"))))))

                   (.remove (dtedss ".labels" "text"))

                   ;; slice to text polylines....

                   (.each
                    (.style
                     (.append
                      (.enter
                       (dss ".lines" "polyline" was))
                      "polyline")
                     "opacity" 0)
                    (fn [d] (set-state! (js* "this") d)))

                   (.attrTween
                    (.style
                     (.duration
                      (.transition
                       (dss ".lines" "polyline" is))
                      duration)
                     "opacity"
                     (fn [d] (if (== (.-value (.-data d)) 0) 0 0.5)))
                    "points"
                    (fn [d]
                      (let [this (js* "this")
                            interpolate (d3/interpolate (get-state this) d)]
                        (fn [t]
                          (let [d2 (interpolate t)]
                            (set-state! this d2)
                            (array
                             (.centroid arc d2)
                             (.centroid outer-arc d2)
                             (array
                              (* radius 0.95 (if (< (mid-angle d2) Math/PI) 1 -1))
                              (second (.centroid outer-arc d2)))))))))
                   
                   (.remove (dtedss ".lines" "polyline"))
                   ))]

    ;;process incoming events...

    (go-loop []
      (when-let [frame (<! data-chan)]
        (change frame)
        (recur)))
    
    ))
