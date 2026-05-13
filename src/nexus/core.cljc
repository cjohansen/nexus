(ns nexus.core
  (:require [clojure.walk :as walk]))

(def conjv (fnil conj []))
(defn intov [a b] (into (vec a) b))

(defn assoc-some [m k v]
  (cond-> m
    (and v (when (coll? v)
             (seq v)))
    (assoc k v)))

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
                        (into (cond-> {:phase phase
                                       :err e
                                       :trace (conjv (:trace ctx) (ctx k))}
                                k (assoc k (ctx k)))
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

(defn interpolate-1 [{:nexus/keys [placeholders]} dispatch-data action]
  (let [interpolations (atom [])
        interpolated (interpolate-walk placeholders interpolations dispatch-data action)]
    (cond-> interpolated
      (not= interpolated action)
      (with-meta {:nexus/action action
                  :nexus/interpolations @interpolations}))))

(defn ^:no-doc wrap-action-handler [f ctx]
  (assoc ctx :actions (apply f (:state ctx) (next (:action ctx)))))

(defn ^:no-doc expand-action [nexus state ctx action]
  (let [[kind :as action] (interpolate-1 nexus (:dispatch-data ctx) action)]
    (if-let [f (get-in nexus [:nexus/actions kind])]
      (let [{:keys [action actions errors]}
            (run-interceptors (-> (select-keys ctx [:errors])
                                  (assoc :state state :action action))
              (conj (vec (:nexus/interceptors nexus))
                    {:phase :expand-action
                     :before-action (partial wrap-action-handler f)})
              [:before-action :after-action :action])
            ctx (cond-> (update ctx :trace conjv action)
                  (seq errors) (assoc :errors errors))]
        (cond
          (empty? actions) (select-keys ctx [:errors :trace])

          (not (actions? actions))
          {:errors [{:action action
                     :phase :expand-action
                     :trace (:trace ctx)
                     :err (ex-info (str (first action) " should expand to a collection of actions")
                                   {:res actions
                                    :action action})}]}

          :else
          (let [res (expand-action nexus state ctx (first actions))
                remaining-actions (intov (:actions res) (next actions))]
            (cond-> res
              (seq remaining-actions) (assoc :actions remaining-actions)))))
      (-> (select-keys ctx [:errors :trace])
          (assoc :effects [action])))))

(defn interpolate
  "Walks `actions`, and replaces any forms matching a registered placeholder with
  the value of calling the corresponding function with `dispatch-data`. Returns
  interpolated `actions`."
  {:arglists '[[nexus dispatch-data actions]]}
  [nexus dispatch-data actions]
  (mapv #(interpolate-1 nexus dispatch-data %) actions))

(defn ^:no-doc get-batched-effects [nexus]
  (->> (:nexus/effects nexus)
       (filter (comp :nexus/batch meta val))
       (mapv key)
       set))

(def effect-ks
  #{:system :dispatch-data :dispatch :errors :results
    ;; For backwards compatibility, see tests for explanation
    :state})

(defn ^:no-doc wrap-batched-effect-handler [f ctx]
  (assoc ctx :res (f (select-keys ctx effect-ks)
                     (:system ctx)
                     (mapv next (:effects ctx)))))

(defn ^:no-doc wrap-effect-handler [f ctx]
  (assoc ctx :res (apply f (select-keys ctx effect-ks)
                         (:system ctx)
                         (next (:effect ctx)))))

(defn ^:no-doc execute-batch [nexus ctx effect-k effects k wrap-handler]
  (if-let [f (get-in nexus [:nexus/effects effect-k])]
    (let [v (cond-> effects
              (= k :effect) first)
          ret (run-interceptors (assoc ctx k v)
                (conj (vec (:nexus/interceptors nexus))
                      {:phase :execute-effect
                       :before-effect (partial wrap-handler f)})
                [:before-effect :after-effect :effect])]
      (cond-> ctx
        (:res ret) (update :results conjv (into {k v} (select-keys ret [:res])))
        (:errors ret) (assoc :errors (:errors ret))))
    (update ctx :errors conjv
            {:phase :execute-effect
             :effect-k effect-k
             :err (ex-info "No such effect" {:available-effects (keys (:nexus/effects nexus))})})))

(defn execute [nexus ctx [effect-k :as effect]]
  {:pre [(action? effect)]}
  (if (get-in nexus [:nexus/effects effect-k])
    (execute-batch nexus ctx effect-k [effect] :effect wrap-effect-handler)
    (update ctx :errors conjv
            {:phase :execute-effect
             :effect-k effect-k
             :err (ex-info "No such effect" {:available-effects (keys (:nexus/effects nexus))})})))

(defn ^{:indent 1 :nodoc true} batch-by [f xs]
  (let [[m order]
        (reduce (fn [[m order] x]
                  (let [k (f x)]
                    [(update m k conjv x)
                     (cond-> order
                       (not (contains? m k)) (conj k))]))
                [{} []]
                xs)]
    (mapv m order)))

(def ^:nodoc divide-by (juxt filter remove))

(defn ^:nodoc ->execute-ctx [ctx dispatch! source]
  (assoc ctx :dispatch
         (fn [actions & [dispatch-data]]
           (-> (dispatch! actions dispatch-data (conjv (:trace ctx) source))
               (select-keys [:results :errors])))))

(defn ^:nodoc dispatch-handler [nexus dispatch! ctx]
  (let [batched? (get-batched-effects nexus)
        get-state (or (some-> (:nexus/system+dispatch-data->state nexus)
                              (partial (:system ctx) (:dispatch-data ctx)))
                      (some-> (:nexus/system->state nexus)
                              (partial (:system ctx)))
                      (constantly nil))]
    (loop [ctx (assoc ctx :state (get-state))
           effects []
           batched-effects []]
      (if-let [effect (first effects)]
        (let [res (execute nexus (->execute-ctx ctx dispatch! effect) effect)]
          (recur
           (-> (assoc ctx :state (get-state))
                       (assoc-some :errors (:errors res))
                       (assoc :results (:results res)))
           (next effects)
           batched-effects))
        (if-let [action (first (:actions ctx))]
          (let [res (expand-action nexus (:state ctx) ctx action)
                [bfxs fxs] (divide-by (comp batched? first) (:effects res))]
            (recur (-> (assoc ctx :trace (:trace res))
                       (update :actions #(into (vec (:actions res)) (next %)))
                       (assoc-some :errors (:errors res)))
                   fxs
                   (into batched-effects bfxs)))
          (-> (reduce
               (fn [ctx batch]
                 (merge ctx (select-keys (execute-batch nexus (->execute-ctx ctx dispatch! effects) (ffirst batch) batch :effects wrap-batched-effect-handler) [:errors :results])))
               ctx
               (batch-by first batched-effects))
              (select-keys [:errors :results :queue :stack])))))))

(defn ^{:indent 3} dispatch [nexus system dispatch-data actions]
  (when (:nexus/actions nexus)
    (assert (or (ifn? (:nexus/system->state nexus))
                (ifn? (:nexus/system+dispatch-data->state nexus)))
            "Either :nexus/system+dispatch-data->state or :nexus/system->state must be a function"))
  (let [dispatch!
        (fn dispatch! [actions & [disp-data trace]]
          (let [handler {:phase :action-dispatch
                         :before-dispatch (partial dispatch-handler nexus dispatch!)}]
            (run-interceptors (cond-> {:system system
                                       :dispatch-data (merge dispatch-data disp-data)
                                       :actions actions}
                                trace (assoc :trace trace))
              (conj (vec (:nexus/interceptors nexus)) handler)
              [:before-dispatch :after-dispatch])))]
    (select-keys (dispatch! actions) [:results :errors])))
