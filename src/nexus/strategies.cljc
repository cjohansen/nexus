(ns nexus.strategies)

(defn abort-on-first-error [k ctx]
  (if (:errors ctx)
    (dissoc ctx :queue :stack :actions :effect :effects)
    ctx))

(def fail-fast
  {:after-action #(abort-on-first-error :after-action %)
   :before-effect #(abort-on-first-error :before-effect %)})
