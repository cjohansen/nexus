(ns nexus.serial
  (:require [nexus.core :as nexus :refer [intov conjv]]))

;;=== Serial dispatch
(defn get-effect-interceptor [nexus effect]
  (when-let [handler (get-in nexus [:nexus/effects (first effect)])]
    (let [wrapped-handler (partial nexus/wrap-effect-handler handler)]
      {:phase :execute-effect
       :before-effect (fn [ctx]
                        (let [{:keys [res errors]} (wrapped-handler ctx)]
                          (cond-> ctx
                            res (update :results conjv {:effect effect :res res})
                            errors (update :errors intov errors))))})))

(defn execute-first [{:nexus/keys [interceptors] :as nexus}
                     {:keys [dispatch-data actions] :as ctx}]
  (let [[effect-k :as effect] (first actions)]
    (if-let [effect-interceptor (get-effect-interceptor nexus effect)]
      (let [{:keys [results errors]}
            (nexus/run-interceptors
             (assoc ctx :effect (nexus/interpolate-1 nexus dispatch-data effect))
             (conjv interceptors effect-interceptor)
             [:before-effect :after-effect :effect])]
        (cond-> (assoc ctx :actions (rest actions))
          (seq results) (update :results conjv results)
          (seq errors) (update :errors conjv errors)))
      ;;Effect not found
      (-> ctx
          (assoc :actions (rest actions))
          (update :errors conjv
                  {:phase :execute-effect
                   :effect-k effect-k
                   :err (ex-info "No such effect"
                                 {:available-effects (keys (:nexus/effects nexus))})})))))

(defn expand-first [{:nexus/keys [interceptors] :as nexus}
                    {:keys [state dispatch-data actions errors] :as ctx}]
  (let [[[kind :as action] & remaining] actions]
    (if-let [handler (get-in nexus [:nexus/actions kind])]
      (let [{:keys [action actions errors]}
            (nexus/run-interceptors
             (cond-> {:state state
                      :action (nexus/interpolate-1 nexus dispatch-data action)}
               errors (assoc :errors errors))
             (conjv interceptors
                    {:id kind
                     :phase :expand-action
                     :before-action (partial nexus/wrap-action-handler handler)})
             [:before-action :after-action :action])
            expansion (intov actions remaining)]
        (cond-> ctx
          (seq errors) (assoc :errors errors)
          (nexus/actions? actions) (assoc :actions expansion
                                          :effects expansion)
          ;;FIXME: This syntax is ugly
          (not (nexus/actions? actions))
          ((fn [ctx*]
             (-> ctx*
                 (assoc :actions remaining)
                 (update-in [:errors] conjv {:action action
                                             :phase :expand-action
                                             :err (ex-info (str (first action) " should expand to a collection of actions")
                                                           {:res actions})}))))))
      (-> ctx
          (assoc :actions remaining
                 :effects actions) ;Think dataspex uses this to track incremental expansion...
          (update-in [:errors] conjv
                     {:action action
                      :phase :expand-action
                      :err (ex-info (str (first action) " not found in handler map")
                                    {:available-actions (keys (:nexus/actions nexus))})})))))

(defn expand-lazily [nexus {:keys [actions] :as ctx}]
  (let [actions* (filterv some? actions)
        ctx* (assoc ctx :actions actions*)
        [[next-kind]] actions*]
    (if (or (empty? actions*) (contains? (:nexus/effects nexus) next-kind))
      ctx*
      (recur nexus (expand-first nexus ctx*)))))

(defn dispatch-serially [nexus {:keys [system dispatch-data] :as ctx}]
    (let [state (apply (:nexus/system->state nexus) [system])
          {:keys [actions] :as ctx*} (->> (assoc ctx :state state)
                                          (expand-lazily nexus))]
      (if (empty? actions)
        ctx*
        (->> (assoc ctx* :dispatch (fn [actions & [additional-dispatch-data]]
                                     (dispatch-serially nexus (assoc ctx
                                                                     :dispatch-data (merge dispatch-data additional-dispatch-data)
                                                                     :actions actions))))
             (execute-first nexus)
             (recur nexus)))))

(defn as-serial-dispatch-interceptor [nexus]
  {:id ::dispatch
   :phase :action-dispatch
   :before-dispatch (partial dispatch-serially nexus)})

(defn dispatch [{:nexus/keys [interceptors] :as nexus} system dispatch-data actions]
  (nexus/run-interceptors {:system system :dispatch-data dispatch-data :actions actions}
    (conjv interceptors (as-serial-dispatch-interceptor nexus))
    [:before-dispatch :after-dispatch]))
