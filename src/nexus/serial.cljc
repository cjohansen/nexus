(ns nexus.serial
  (:require [nexus.action-log]
            [nexus.inspector]
            [nexus.core :as nexus :refer [intov conjv]]))


;;=== Dataspex integration
;; This is intended to allows the dataspex action log to be enabled at runtime via the js console
;; It's NOT exported here -- the decision belongs in userland -- but enable-dataspex! can be
;; called from an exported function in the application namespace

(defonce !dataspex-interceptor (atom nil))

(defn enable-dataspex! []
  (when (nil? @!dataspex-interceptor)
    (let [log (nexus.action-log/create-log)]
      (nexus.action-log/install-inspector log)
      (reset! !dataspex-interceptor (nexus.inspector/get-interceptor log)))))

(defn inject-dataspex? [nexus-map]
  (cond-> nexus-map
    @!dataspex-interceptor (update :nexus/interceptors conjv @!dataspex-interceptor)))

;;=== Serial dispatch
(defn get-effect-interceptor [nexus-map effect]
  (when-let [handler (get-in nexus-map [:nexus/effects (first effect)])]
    (let [wrapped-handler (partial nexus/wrap-effect-handler handler)]
      {:phase :execute-effect
       :before-effect (fn [ctx]
                        (let [{:keys [res errors]} (wrapped-handler ctx)]
                          (cond-> ctx
                            res (update :results conjv {:effect effect :res res})
                            errors (update :errors intov errors))))})))

(defn execute-first [{:nexus/keys [interceptors] :as nexus-map} {:keys [dispatch-data actions] :as ctx}]
  (let [[effect-id & _effect-params :as effect] (first actions)]
    (if-let [effect-interceptor (get-effect-interceptor nexus-map (first actions))]
      (let [effect-interpolated (nexus/interpolate-1 nexus-map dispatch-data effect)
            ictx (assoc ctx :effect effect-interpolated)
            {:keys [results errors]} (nexus/run-interceptors ictx (conjv interceptors effect-interceptor) [:before-effect :after-effect :effect])]
        (cond-> (assoc ctx :actions (rest actions))
          (seq results) (update :results conjv results)
          (seq errors) (update :errors conjv errors)))
      ;;Effect not found
      (-> ctx
          (assoc :actions (rest actions))
          (update :errors conjv {:phase :execute-effect
                                 :effect-k effect-id
                                 :err (ex-info "No such effect" {:available-effects (keys (:nexus/effects nexus-map))})})))))

(defn expand-first [{:nexus/keys [interceptors] :as nexus-map} {:keys [state dispatch-data actions errors] :as ctx}]
  (let [[first-action & remaining] actions
        [action-id & _action-params :as action] first-action]
    (if-let [handler (get-in nexus-map [:nexus/actions action-id])]
      (let [action-interpolated (nexus/interpolate-1 nexus-map dispatch-data action)
            action-interceptor {:id action-id
                                :phase :expand-action
                                :before-action (partial nexus/wrap-action-handler handler)}
            {:keys [action actions errors]} (nexus/run-interceptors
                                             (cond-> {:state state :action action-interpolated}
                                               errors (assoc :errors errors))
                                             (conjv interceptors action-interceptor)
                                             [:before-action :after-action :action])]
        (cond-> (assoc ctx :actions (intov actions remaining))
          (seq errors) (assoc :errors errors)
          ;;TODO: Do we want to prevent prefixing the returned actions if they're invalid?
          (not (nexus/actions? actions)) (update-in [:errors] conjv {:action action
                                                                     :phase :expand-action
                                                                     :err (ex-info (str (first action) " should expand to a collection of actions")
                                                                                   {:res actions})})))
      (-> ctx
          (assoc :actions remaining)
          (update-in [:errors] conjv
                     {:action action
                      :phase :expand-action
                      :err (ex-info (str (first action) " not found in handler map")
                                    {:available-actions (keys (:nexus/actions nexus-map))})})))))

(defn expand-lazily [nexus-map {:keys [actions] :as ctx}]
  (let [actions* (filterv some? actions)
        ctx* (assoc ctx :actions actions*)
        [[next-id & _next-params] & _remaining] actions*]
    (if (or (empty? actions*) (contains? (:nexus/effects nexus-map) next-id))
      ctx*
      (recur nexus-map (expand-first nexus-map ctx*)))))

(defn dispatch-serial-base [nexus-map {:keys [system dispatch-data] :as ctx}]
  (let [state (apply (:nexus/system->state nexus-map) [system])
        {:keys [actions] :as ctx*} (->> (assoc ctx :state state)
                                        (expand-lazily nexus-map))]
    (if (empty? actions)
      ctx*
      (->> (assoc ctx* :dispatch (fn [actions & [additional-dispatch-data]]
                                   (dispatch-serial-base nexus-map (assoc ctx
                                                                          :dispatch-data (merge dispatch-data additional-dispatch-data)
                                                                          :actions actions))))
           (execute-first nexus-map)
           (recur nexus-map)))))

(defn as-serial-dispatch-interceptor [nexus-map]
  {:id ::dispatch
   :phase :action-dispatch
   :before-dispatch (partial dispatch-serial-base nexus-map)})

(defn dispatch [nexus-map system dispatch-data actions]
  (let [{:nexus/keys [interceptors] :as nexus-map*} (inject-dataspex? nexus-map)
        dispatch-interceptor (as-serial-dispatch-interceptor nexus-map*)
        ctx {:system system :dispatch-data dispatch-data :actions actions}]
    (nexus/run-interceptors ctx (conjv interceptors dispatch-interceptor) [:before-dispatch :after-dispatch])))
