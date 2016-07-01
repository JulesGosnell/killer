(ns killer.datatable
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :as async  :refer (<! >! put! chan)]
   [jQuery :as jQuery]
   [DataTable :as DataTable]
   ))

(defn render [root duration data-chan]
  (let [table (.DataTable (js/$ root) (js-obj))
        change (fn [rows]
                 (js/console.log "TABLE:" table "ROWS:" rows)
                 (.draw (.add (.-rows (.clear table)) rows))
                 )]
    
    ;;process incoming events...

    (go-loop []
      (when-let [frame (<! data-chan)]
        (change frame)
        (recur)))

    ))
