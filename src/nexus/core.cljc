(ns nexus.core
  (:require [clojure.walk :as walk]))

(def ^:private conjv (fnil conj []))
(def ^:private intov (fnil into []))

(defn action? [data]
  (and (vector? data) (keyword? (first data))))

(defn actions? [data]
  (and (sequential? data) (every? action? data)))

(defn ^{:indent 1 :no-doc true} run-interceptors [ctx interceptors [before after k]]
  (letfn [(invoke [f state phase interceptor]
            (try
              (cond-> state
                (ifn? f) f)
              (catch #?(:clj Exception :cljs :default) e
                (update state :errors conjv
                        (into {:phase phase
                               :err e
                               k (ctx k)}
                              (select-keys interceptor [:id]))))))]
    (loop [state (merge ctx {:queue interceptors :stack ()})]
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

(defn ^:no-doc wrap-action-handler [f ctx]
  (assoc ctx :actions (apply f (:state ctx) (next (:action ctx)))))

(defn ^:no-doc expand-action [nexus state [kind :as action] errors]
  (if-let [f (get-in nexus [:actions kind])]
    (let [{:keys [action actions errors]}
          (run-interceptors (cond-> {:state state :action action}
                              errors (assoc :errors errors))
            (conj (vec (:interceptors nexus))
                  {:phase :expand-action
                   :before-action (partial wrap-action-handler f)})
            [:before-action :after-action :action])
          acc (cond-> {}
                (seq errors) (assoc :errors errors))]
      (cond
        (nil? actions) acc

        (not (actions? actions))
        {:errors [{:action action
                   :phase :expand-action
                   :err (ex-info (str (first action) " should expand to a collection of actions")
                                 {:res actions})}]}

        (= actions [action])
        (cond-> acc
          (seq actions) (assoc :actions actions))

        :else
        (reduce (fn [res action]
                  (let [{:keys [errors actions]} (expand-action nexus state action (:errors res))]
                    (cond-> res
                      (seq errors) (update :errors into errors)
                      (seq actions) (update :actions into actions))))
                acc actions)))
    {:actions [action]}))

(defn expand-actions
  "Loops over `actions`, and expands each action to a list of actions with
  available implementations in `nexus`. Passes `state` to each implementation.
  Calls every available `:before-action` interceptor before expanding and every
  `:after-action` interceptor after. Returns a map of `{:effects :errors}`."
  [nexus state actions]
  (reduce (fn [res action]
            (let [{:keys [actions errors]} (expand-action nexus state action (:errors res))]
              (cond-> res
                (seq actions) (update :effects into actions)
                (seq errors) (assoc :errors errors))))
          {} actions))

(defn interpolate
  "Walks `actions`, and replaces any forms matching a registered placeholder with
  the value of calling the corresponding function with `dispatch-data`. Returns
  interpolated `actions`."
  {:arglists '[[nexus dispatch-data actions]]}
  [{:keys [placeholders]} dispatch-data actions]
  (walk/postwalk
   (fn [x]
     (if-let [f (when (vector? x)
                  (get placeholders (first x)))]
       (apply f dispatch-data (next x))
       x))
   actions))

(defn ^:no-doc get-batched-effects [nexus]
  (->> (:effects nexus)
       (filter (comp :nexus/batch meta val))
       (mapv key)
       set))

(defn ^:no-doc wrap-batched-effect-handler [f ctx]
  (assoc ctx :res (f (dissoc ctx :system :actions :effects :queue :stack)
                     (:system ctx)
                     (mapv next (:effects ctx)))))

(defn ^:no-doc wrap-effect-handler [f ctx]
  (assoc ctx :res (apply f (dissoc ctx :system :actions :effect :queue :stack)
                         (:system ctx)
                         (next (:effect ctx)))))

(defn ^:no-doc execute-batch [acc nexus ctx effect-k effects k wrap-handler]
  (if-let [f (get-in nexus [:effects effect-k])]
    (let [v (cond-> effects
              (= k :effect) first)
          ret (run-interceptors (into (assoc ctx k v) (select-keys acc [:errors]))
                (conj (vec (:interceptors nexus))
                      {:phase :execute-effect
                       :before-effect (partial wrap-handler f)})
                [:before-effect :after-effect :effect])]
      (cond-> acc
        (:res ret) (update :results conjv (into {k v} (select-keys ret [:res])))
        (:errors ret) (assoc :errors (:errors ret))))
    (update acc :errors conjv
            {:phase :execute-effect
             :effect-k effect-k
             :err (ex-info "No such effect" {:available-effects (keys (:effects nexus))})})))

(defn execute
  "Execute `effects` one by one. Calls every `:before-effect` interceptor before
  executing the action, and every `:after-effect` interceptor after. Returns a
  collection of maps of `{:action :res}` where `:res` is the return value of the
  effect implementation."
  [nexus ctx effects]
  (->> (group-by (comp (get-batched-effects nexus) first) effects)
       (reduce
        (fn [acc [effect-k effects]]
          (if effect-k
            (execute-batch acc nexus ctx effect-k effects :effects wrap-batched-effect-handler)
            (reduce #(execute-batch %1 nexus ctx (first %2) [%2] :effect wrap-effect-handler)
                    acc effects))) {})))

(defn ^{:indent 3} dispatch [nexus system dispatch-data actions]
  (when (:actions nexus)
    (assert (ifn? (:system->state nexus)) ":system->state must be a function"))
  (let [dispatch!
        (fn dispatch! [actions & [disp-data]]
          (let [handler {:phase :action-dispatch
                         :before-dispatch
                         (fn [ctx]
                           (let [actions (interpolate nexus (:dispatch-data ctx) (:actions ctx))
                                 {:keys [effects errors]} (expand-actions nexus ((:system->state nexus) (:system ctx)) actions)]
                             (cond-> ctx
                               errors (assoc :errors errors)
                               effects (into (execute nexus (assoc (dissoc ctx :actions) :dispatch dispatch!)
                                                      (cond->> effects
                                                        (not= actions effects)
                                                        (interpolate nexus (:dispatch-data ctx))))))))}]
            (run-interceptors {:system system
                               :dispatch-data (merge dispatch-data disp-data)
                               :actions actions}
              (conj (vec (:interceptors nexus)) handler)
              [:before-dispatch :after-dispatch])))]
    (select-keys (dispatch! actions) [:results :errors])))
