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

(defn ^{:indent 2} with-interceptor [nexus phase f & [id]]
  (update nexus :nexus/interceptors (fnil conj []) (cond-> {phase f}
                                                     id (assoc :id id))))

(defn log-interceptor [log n]
  {:id n
   :before-action (fn [in]
                    (swap! log conj [:before-action n (get-in in [:action 0])])
                    in)
   :after-action (fn [in]
                   (swap! log conj (cond-> [:after-action n (get-in in [:action 0])]
                                     (seq (:actions in)) (conj (mapv first (:actions in)))))
                   in)
   :before-effect (fn [in]
                    (swap! log conj [:before-effect n
                                     (first (or (:effect in) (first (:effects in))))])
                    in)
   :after-effect (fn [in]
                   (swap! log conj [:after-effect n
                                    (first (or (:effect in) (first (:effects in))))
                                    (:res in)])
                   in)
   :before-dispatch (fn [in]
                      (swap! log conj [:before-dispatch n (:actions in)])
                      in)
   :after-dispatch (fn [in]
                     (swap! log conj [:after-dispatch n (:results in)])
                     in)})
