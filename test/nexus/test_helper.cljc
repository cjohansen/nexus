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

(defonce !last-dispatch-order (atom []))
(defonce !dispatch-history (atom []))

(def dispatch-history-interceptor
  {:id ::dispatch-order
   :before-dispatch (fn [{:keys [actions] :as ctx}]
                      (swap! !dispatch-history conj [:dispatch actions])
                      (reset! !last-dispatch-order [])
                      ctx)
   :after-dispatch (fn [ctx]
                     (swap! !dispatch-history into @!last-dispatch-order)
                     ctx)
   :before-action (fn [{:keys [action] :as ctx}]
                    (swap! !last-dispatch-order conj [:expand-action action])
                    ctx)
   :before-effect (fn [{:keys [effect] :as ctx}]
                    (swap! !last-dispatch-order conj [:exec-effect effect])
                    ctx)})

(def nexus
  {:nexus/system->state deref

   :nexus/interceptors [dispatch-history-interceptor]

   :nexus/placeholders
   {:interpolate/me
    (fn [_]
      :interpolated)

    :interpolation/order
    (fn [_]
      [:interpolated/during (count @!last-dispatch-order)])}

   :nexus/effects
   {:fx1 (fn [_ctx _sys] nil)
    :fx2 (fn [_ctx _sys] nil)
    :fx3 (fn [_ctx _sys] nil)
    :fx4 (fn [_ctx _sys] nil)}

   :nexus/actions
   {:ax1 (fn [_state & _opts]
           [[:fx1]
            [:fx2]])

    :ax2 (fn [_state & _opts]
           [[:fx3]
            [:fx4]])}})

(defn test-dispatch-order [f actions]
  (let [!store (atom {:executions 0})]
    (f nexus !store {} actions)
    @!last-dispatch-order))
