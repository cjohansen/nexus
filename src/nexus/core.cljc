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

(defn ^:no-doc expand-action [nexus state [kind :as action]]
  (if-let [f (get-in nexus [:actions kind])]
    (let [{:keys [action actions errors]}
          (run-interceptors {:state state :action action}
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
                  (let [{:keys [errors actions]} (expand-action nexus state action)]
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
            (let [{:keys [actions errors]} (expand-action nexus state action)]
              (cond-> res
                (seq actions) (update :effects into actions)
                (seq errors) (update :errors intov errors))))
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
