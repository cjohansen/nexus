(ns nexus.test-helper
  (:require [clojure.walk :as walk]))

(defn get-message [e]
  #?(:clj (.getMessage e)
     :cljs (.-message e)))

(defn ex->data [e]
  {:message (get-message e)
   :data (walk/prewalk
          (fn [x]
            (cond
              (and (:err x) (not (map? (:err x))))
              (let [data (ex-data (:err x))]
                (assoc x :err
                       (cond-> {:message (get-message (:err x))}
                         (not-empty data) (assoc :data data))))

              (fn? x)
              ::fn

              :else x))
          (ex-data e))})

(defn datafy-errors [res]
  (update res :errors (fn [errors] (mapv #(update % :err ex->data) errors))))
