(ns nexus.strategies)

(defn abort-on-first-error [k ctx]
  (prn k (or (:action ctx) (:effect ctx)) (keys ctx))
  (if (:errors ctx)
    (dissoc ctx :queue :stack :actions :effect :effects)
    ctx))

(def fail-fast
  {:after-action #(abort-on-first-error :after-action %)
   :before-effect #(abort-on-first-error :before-effect %)})
