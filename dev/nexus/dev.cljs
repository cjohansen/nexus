(ns nexus.dev
  (:require [counter.core :as counter]
            [dataspex.core :as dataspex]
            [nexus.action-log :as action-log]))

(def system (atom {:number 0, :step-size 1}))
(def el (js/document.getElementById "app"))
(dataspex/inspect "App state" system)

(defn ^:export main []
  (let [log (action-log/create-log)]
    (-> (action-log/install-logger counter/nexus log)
        (counter/start el system))))

(defn ^:dev/after-load refresh []
  (swap! system assoc ::now (js/Date.)))
