(ns nexus.serial.dispatch
  (:require [nexus.core :as nexus :refer [intov conjv]]))

(defn get-effect-interceptor [nexus effect]
  (when-let [handler (get-in nexus [:nexus/effects (first effect)])]
    {:phase :execute-effect
     :before-effect (partial nexus/wrap-effect-handler handler)}))

(defn ^{:indent 1} execute-first [{:nexus/keys [interceptors] :as nexus}
                                  {:keys [dispatch-data actions] :as ctx}]
  (let [[effect-k :as effect] (first actions)]
    (if-let [effect-interceptor (get-effect-interceptor nexus effect)]
      (let [effect (nexus/interpolate-1 nexus dispatch-data effect)
            {:keys [res errors]} (nexus/run-interceptors (assoc ctx :effect effect)
                                   (conjv interceptors effect-interceptor)
                                   [:before-effect :after-effect :effect])]
        (cond-> (assoc ctx :actions (rest actions))
          res (update :results conjv {:effect effect :res res})
          (seq errors) (update :errors intov errors)))
      ;; Effect not found
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
                     {:phase :expand-action
                      :before-action (partial nexus/wrap-action-handler handler)})
              [:before-action :after-action :action])
            expansion (intov actions remaining)
            ok? (or (nil? actions) (nexus/actions? actions))]
        (cond-> ctx
          (seq errors) (assoc :errors errors)
          ok? (assoc :actions expansion)
          (not ok?)
          (-> (assoc :actions remaining)
              (update-in [:errors] conjv {:action action
                                          :phase :expand-action
                                          :err (ex-info (str (first action) " should expand to a collection of actions")
                                                        {:res actions})}))))
      (-> ctx
          (assoc :actions remaining
                 :effects actions) ;; Think dataspex uses this to track incremental expansion...
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

(declare dispatch)

(defn dispatch-serially [nexus {:keys [system] :as ctx}]
  (let [state (apply (:nexus/system->state nexus) [system])
        ctx* (->> (assoc ctx :state state)
                  (expand-lazily nexus))]
    (if (empty? (:actions ctx*))
      (select-keys ctx* [:results :errors :queue :stack :dispatch])
      (->> ctx*
           (execute-first nexus)
           (recur nexus)))))

(defn dispatch [{:nexus/keys [interceptors] :as nexus} system dispatch-data actions]
  (let [dispatch* (fn [actions & [additional-dispatch-data]]
                    (dispatch nexus system (merge dispatch-data additional-dispatch-data) actions))
        interceptors (conjv interceptors {:id ::dispatch
                                          :phase :action-dispatch
                                          :before-dispatch (partial dispatch-serially nexus)})]
    (-> (nexus/run-interceptors {:system system :dispatch dispatch* :dispatch-data dispatch-data :actions actions}
                                interceptors
                                [:before-dispatch :after-dispatch])
        (dissoc :queue :stack :dispatch))))
