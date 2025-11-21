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
  (-> res
      (select-keys [:effects :errors])
      (update :errors (fn [errors] (mapv #(update % :err ex->data) errors)))))


;; Interceptor to verify action expansion and effect execution order for default and serial dispatch strategies
(defonce !dispatch-history (atom []))

(def dispatch-order-interceptor
  {:id ::dispatch-order
   :before-dispatch (fn [{:keys [actions] :as ctx}] (reset! !dispatch-history [[:dispatch actions]]) ctx)
   :before-action (fn [{:keys [action] :as ctx}] (swap! !dispatch-history conj [:expand-action action]) ctx)
   :before-effect (fn [{:keys [effect] :as ctx}]  (swap! !dispatch-history conj [:exec-effect effect]))})
