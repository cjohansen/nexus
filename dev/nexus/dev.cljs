(ns nexus.dev
  (:require [counter.core :as counter]
            [dataspex.core :as dataspex]
            [nexus.action-log :as action-log]))

(def store (atom {:number 0, :step-size 1}))
(def el (js/document.getElementById "app"))
(dataspex/inspect "App state" store)
(action-log/inspect)

(defn ^:export main []
  (let [log (action-log/create-log)]
    (action-log/install-inspector log)
    (-> (action-log/install-logger counter/nexus log)
        (counter/start el store))))
