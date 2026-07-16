(ns nexus.core
  (:require [clojure.walk :as walk]))

(def ^:no-doc conjv (fnil conj []))

(defn action? [data]
  (and (vector? data) (keyword? (first data))))

(defn actions? [data]
  (and (sequential? data) (every? action? data)))

(defn log-error [nexus ctx error]
  (when-let [on-error (:nexus/on-error nexus)]
    (try
      (on-error (dissoc ctx :stack :queue) error)
      (catch #?(:clj Exception :cljs :default) _
        ;; Well, you had your chance!
        )))
  error)

(defn ^{:no-doc true :indent 1} try-f [ctx f & [data]]
  (try
    (cond-> ctx
      (ifn? f) f)
    (catch #?(:clj Exception :cljs :default) e
      (update ctx :errors conjv
              (->> (assoc data
                          :err e
                          :trace (:trace ctx))
                   (log-error (:nexus ctx) ctx))))))

(defn ^{:indent 1 :no-doc true} run-interceptors [ctx interceptors [before after k]]
  (letfn [(invoke [f state phase interceptor]
            (->> (select-keys interceptor [:id])
                 (into (cond-> {:phase phase}
                         k (assoc k (get ctx k))))
                 (try-f state f)))]
    (loop [state (assoc ctx :queue interceptors :stack ())]
      (cond
        (:queue state)
        (let [interceptor (first (:queue state))
              state (-> (update state :queue next)
                        (update :stack conj interceptor))]
          (recur (invoke (get interceptor before) state (or (:phase interceptor) before) interceptor)))

        (:stack state)
        (let [interceptor (first (:stack state))
              state (update state :stack next)]
          (recur (invoke (get interceptor after) state after interceptor)))

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

(defn expand-actions
  "Loops over `actions`, and expands each action to a list of actions with
  available implementations in `nexus`. Passes `state` to each implementation.
  Returns a map of `{:effects :errors}`."
  [nexus state actions & [dispatch-data]]
  (loop [ctx {}
         actions actions
         trace []]
    (if (empty? actions)
      ctx
      (let [[kind :as action] (interpolate-1 nexus dispatch-data (first actions))
            next-actions (next actions)]
        (if-let [f (or (get-in nexus [:nexus/expansions kind])
                       (get-in nexus [:nexus/actions kind]))]
          (let [[actions err] (try
                                [(apply f state (next action))]
                                (catch #?(:clj Exception :cljs :default) e
                                  [nil e]))]
            (cond
              err
              (recur (->> {:action action
                           :phase :expand-action
                           :err err
                           :trace (conj trace action)}
                          (update ctx :errors conj))
                     next-actions trace)

              (nil? actions)
              (recur ctx next-actions trace)

              (not (actions? actions))
              (recur (->> {:errors [{:action action
                                     :phase :expand-action
                                     :err (ex-info (str kind " should expand to a collection of actions")
                                                   {:res actions})}]})
                     next-actions trace)

              :else
              (recur ctx (vec (concat actions next-actions)) (conj trace action))))
          (recur (update ctx :effects conjv action) next-actions trace))))))

(defn ^:no-doc get-state [nexus ctx]
  (or (when-let [f (:nexus/system+dispatch-data->state nexus)]
        (f (:system ctx) (:dispatch-data ctx)))
      (when-let [f (:nexus/system->state nexus)]
        (f (:system ctx)))))

(defn ^:no-doc reset-ctx-nesting [{:keys [queue stack trace]} ctx]
  (assoc ctx :queue queue :stack stack :trace trace))

(declare dispatch-actions)

(defn ^:no-doc expand-and-dispatch-action [nexus dispatch! ctx action-f action]
  (try-f ctx
    (fn [ctx]
      (let [actions (apply action-f (:state ctx) (next action))]
        (cond
          (empty? actions)
          (assoc ctx :actions [])

          (not (actions? actions))
          (update ctx :errors conjv
                  (->> {:action action
                        :phase :expand-action
                        :trace (:trace ctx)
                        :err (ex-info (str (first action) " should expand to a collection of actions")
                                      {:res actions
                                       :action action})}
                       (log-error nexus ctx)))

          :else
          (->> (assoc ctx :actions actions)
               (dispatch-actions nexus dispatch!)))))
    {:action action
     :phase :expand-action}))

(defn ^:no-doc ->execute-ctx [ctx]
  (update ctx :dispatch
          (fn [dispatch]
            (fn [actions & [dispatch-data]]
              (-> (dispatch actions dispatch-data (dissoc ctx :action :actions :effect :stack :queue))
                  (select-keys [:results :errors]))))))

(defn execute-effect [nexus dispatch! ctx effect-f effect]
  (-> (assoc ctx :dispatch dispatch! :effect effect)
      (run-interceptors
          (conj (:nexus/interceptors nexus)
                {:phase :execute-effect
                 :before-effect
                 (fn [{:keys [system effect] :as ctx*}]
                   (let [result (apply effect-f (->execute-ctx ctx*) system (next effect))]
                     (-> (assoc ctx* :res result :state (get-state nexus ctx*))
                         (update :results conjv {:effect effect :res result}))))})
        [:before-effect :after-effect :effect])
      (dissoc :effect :res)))

(defn ^:no-doc dispatch-action [nexus dispatch! ctx action]
  (run-interceptors (update (assoc ctx :action action) :trace conjv action)
    (conj (:nexus/interceptors nexus)
          {:phase :expand-action
           :before-action
           (fn [ctx*]
             (let [[action-k :as action] (interpolate-1 nexus (:dispatch-data ctx*) (:action ctx*))]
               (or
                (when-let [action-f (get-in nexus [:nexus/expansions action-k])]
                  (-> (->> (expand-and-dispatch-action nexus dispatch! ctx* action-f action)
                           (reset-ctx-nesting ctx*))
                      (assoc :action action)))
                (when-let [effect-f (get-in nexus [:nexus/effects action-k])]
                  (->> (execute-effect nexus dispatch! ctx* effect-f action)
                       (reset-ctx-nesting ctx*)))
                (update ctx* :errors conjv
                        (->> {:phase :execute-effect
                              :effect-k action-k
                              :err (ex-info (str "No effect handler for " action-k) {:available-effects (keys (:nexus/effects nexus))})}
                             (log-error nexus ctx*))))))})
    [:before-action :after-action :action]))

(defn dispatch-actions [nexus dispatch! {:keys [queue stack] :as ctx}]
  (-> (reduce
       (fn [ctx action]
         (dispatch-action nexus dispatch! ctx action))
       (assoc ctx :state (get-state nexus ctx))
       (:actions ctx))
      (assoc :queue queue :stack stack)))

(defn ^{:indent 3} dispatch [nexus system dispatch-data actions]
  (let [nexus (-> nexus
                  (update :nexus/expansions #(or % (:nexus/actions nexus)))
                  (update :nexus/interceptors vec))
        dispatch!
        (fn dispatch! [actions & [disp-data parent-ctx]]
          (let [handler {:phase :action-dispatch
                         :before-dispatch (partial dispatch-actions nexus dispatch!)}]
            (run-interceptors (assoc parent-ctx
                                     :nexus nexus
                                     :system system
                                     :dispatch-data (merge dispatch-data disp-data)
                                     :actions actions)
              (conj (:nexus/interceptors nexus) handler)
              [:before-dispatch :after-dispatch])))]
    (when (:nexus/expansions nexus)
      (assert (or (ifn? (:nexus/system->state nexus))
                  (ifn? (:nexus/system+dispatch-data->state nexus)))
              "Either :nexus/system+dispatch-data->state or :nexus/system->state must be a function"))
    (dissoc (dispatch! actions) :nexus :system :state :trace :queue :stack :dispatch :dispatch-data :action :actions)))
