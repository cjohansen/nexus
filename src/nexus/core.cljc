(ns nexus.core
  (:require [clojure.walk :as walk]))

(def ^:nodoc conjv (fnil conj []))

(defn action? [data]
  (and (vector? data) (keyword? (first data))))

(defn actions? [data]
  (and (sequential? data) (every? action? data)))

(defn ^:no-doc get-batched-effects [nexus]
  (->> (:nexus/effects nexus)
       (filter (comp :nexus/batch meta val))
       (mapv key)
       set))

(defn log-error [nexus ctx error]
  (when-let [on-error (:nexus/on-error nexus)]
    (on-error (dissoc ctx :stack :queue) error)
    (try
      (catch #?(:clj Exception :cljs :default) _
        ;; Well, you had your chance!
        )))
  error)

(defn ^{:indent 1 :no-doc true} run-interceptors [ctx interceptors [before after k]]
  (letfn [(invoke [f state phase interceptor]
            (try
              (cond-> state
                (ifn? f) f)
              (catch #?(:clj Exception :cljs :default) e
                (update state :errors conjv
                        (->> (select-keys interceptor [:id])
                             (into (cond-> {:phase phase
                                            :err e
                                            :trace (:trace state)}
                                     k (assoc k (ctx k))))
                             (log-error (:nexus ctx) ctx))))))]
    (loop [state (cond-> (assoc ctx :queue interceptors :stack ())
                   k (update :trace conjv (ctx k)))]
      (cond
        (:queue state)
        (let [interceptor (first (:queue state))
              state (-> (update state :queue next)
                        (update :stack conj interceptor))]
          (recur (invoke (before interceptor) state (or (:phase interceptor) before) interceptor)))

        (:stack state)
        (let [interceptor (first (:stack state))
              state (update state :stack next)]
          (recur (invoke (after interceptor) state after interceptor)))

        :else state))))

(defn ^:no-doc interpolate-walk [placeholders interpolations dispatch-data x]
  (if (-> x meta :nexus/skip-interpolation)
    x
    (let [x' (if (coll? x)
               (walk/walk #(interpolate-walk placeholders interpolations dispatch-data %) identity x)
               x)]
      (if-let [f (when (vector? x')
                   (get placeholders (first x')))]
        (let [resolution (apply f dispatch-data (next x'))]
          (swap! interpolations conj {:placeholder x'
                                      :resolution resolution})
          resolution)
        x'))))

(defn ^:no-doc interpolate-1 [{:nexus/keys [placeholders]} dispatch-data action]
  (let [interpolations (atom [])
        interpolated (interpolate-walk placeholders interpolations dispatch-data action)]
    (cond-> interpolated
      (not= interpolated action)
      (with-meta {:nexus/action action
                  :nexus/interpolations @interpolations}))))

(defn interpolate
  "Walks `actions`, and replaces any forms matching a registered placeholder with
  the value of calling the corresponding function with `dispatch-data`. Returns
  interpolated `actions`."
  {:arglists '[[nexus dispatch-data actions]]}
  [nexus dispatch-data actions]
  (mapv #(interpolate-1 nexus dispatch-data %) actions))

(defn ^:nodoc get-state [nexus ctx]
  (or (when-let [f (:nexus/system+dispatch-data->state nexus)]
        (f (:system ctx) (:dispatch-data ctx)))
      (when-let [f (:nexus/system->state nexus)]
        (f (:system ctx)))))

(defn ^:nodoc reset-ctx-nesting [{:keys [queue stack trace]} ctx]
  (assoc ctx :queue queue :stack stack :trace trace))

(declare dispatch-actions)

(defn ^:nodoc dispatch-action [nexus dispatch! ctx action-f action]
  (run-interceptors (assoc ctx :action action)
    (conj (vec (:nexus/interceptors nexus))
          {:phase :expand-action
           :before-action
           (fn [ctx*]
             (let [actions (apply action-f (:state ctx*) (next (:action ctx*)))]
               (cond
                 (empty? actions)
                 (reset-ctx-nesting ctx (assoc ctx* :actions []))

                 (not (actions? actions))
                 (update ctx* :errors conjv
                         (->> {:action action
                               :phase :expand-action
                               :trace (:trace ctx*)
                               :err (ex-info (str (first action) " should expand to a collection of actions")
                                             {:res actions
                                              :action action})}
                              (log-error nexus ctx)))

                 :else
                 (->> (assoc ctx* :actions actions)
                      (dispatch-actions nexus dispatch!)
                      (reset-ctx-nesting ctx)))))})
    [:before-action :after-action :action]))

(defn ^:nodoc ->execute-ctx [ctx]
  (update ctx :dispatch
          (fn [dispatch]
            (fn [actions & [dispatch-data]]
              (-> (dispatch actions dispatch-data (dissoc ctx :action :actions :effect :stack :queue))
                  (select-keys [:results :errors]))))))

(defn execute-effect [nexus dispatch! ctx effect-f effect]
  (-> (assoc ctx :dispatch dispatch!)
      (assoc :effect effect :dispatch dispatch!)
      (run-interceptors
          (conj (vec (:nexus/interceptors nexus))
                {:phase :execute-effect
                 :before-effect
                 (fn [{:keys [system effect] :as ctx*}]
                   (let [result (apply effect-f (->execute-ctx ctx*) system (next effect))]
                     (-> (assoc ctx* :res result :state (get-state nexus ctx*))
                         (update :results conjv {:effect effect :res result}))))})
        [:before-effect :after-effect :effect])
      (dissoc :effect :res)))

(defn dispatch-actions [nexus dispatch! {:keys [queue stack] :as ctx}]
  (-> (reduce
       (fn [ctx action]
         (let [[action-k :as action] (interpolate-1 nexus (:dispatch-data ctx) action)]
           (or
            (when-let [action-f (get-in nexus [:nexus/actions action-k])]
              (dispatch-action nexus dispatch! ctx action-f action))
            (when-let [effect-f (get-in nexus [:nexus/effects action-k])]
              (execute-effect nexus dispatch! ctx effect-f action))
            (update ctx :errors conjv
                    (->> {:phase :execute-effect
                          :effect-k action-k
                          :err (ex-info "No such effect" {:available-effects (keys (:nexus/effects nexus))})}
                         (log-error nexus ctx))))))
       (assoc ctx :state (get-state nexus ctx))
       (:actions ctx))
      (assoc :queue queue :stack stack)))

(defn ^{:indent 3} dispatch [nexus system dispatch-data actions]
  (when (:nexus/actions nexus)
    (assert (or (ifn? (:nexus/system->state nexus))
                (ifn? (:nexus/system+dispatch-data->state nexus)))
            "Either :nexus/system+dispatch-data->state or :nexus/system->state must be a function"))
  (let [dispatch!
        (fn dispatch! [actions & [disp-data parent-ctx]]
          (let [handler {:phase :action-dispatch
                         :before-dispatch (partial dispatch-actions nexus dispatch!)}]
            (run-interceptors (assoc parent-ctx
                                     :nexus nexus
                                     :system system
                                     :dispatch-data (merge dispatch-data disp-data)
                                     :actions actions)
              (conj (vec (:nexus/interceptors nexus)) handler)
              [:before-dispatch :after-dispatch])))]
    (dissoc (dispatch! actions) :nexus :system :state :trace :queue :stack :dispatch :dispatch-data :action :actions)))
