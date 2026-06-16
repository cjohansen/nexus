(ns nexus.strategies)

(defn abort-on-first-error [ctx]
  (if (:errors ctx)
    (dissoc ctx :queue :stack :actions :effect :effects)
    ctx))

(def fail-fast
  {:before-action abort-on-first-error
   :before-effect abort-on-first-error})
