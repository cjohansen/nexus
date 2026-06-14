(ns nexus.batching
  (:require [nexus.core :as nexus]))

(def conjv (fnil conj []))

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

(defn ^:no-doc execute-batch [nexus ctx effect-k effects k wrap-handler]
  (if-let [f (get-in nexus [:nexus/effects effect-k])]
    (let [v (cond-> effects
              (= k :effect) first)
          ret (nexus/run-interceptors (assoc ctx k v)
                (conj (vec (:nexus/interceptors nexus))
                      {:phase :execute-effect
                       :before-effect (partial wrap-handler f)})
                [:before-effect :after-effect :effect])]
      (cond-> (dissoc ret :res)
        (:res ret) (update :results conjv (into {k v} (select-keys ret [:res])))))
    (update ctx :errors conjv
            {:phase :execute-effect
             :effect-k effect-k
             :err (ex-info "No such effect" {:available-effects (keys (:nexus/effects nexus))})})))

(defn ^:no-doc wrap-batched-effect-handler [f ctx]
  (assoc ctx :res (f ctx
                     (:system ctx)
                     (mapv next (:effects ctx)))))

(defn before-effect [nexus ctx]
  (let [[effect-k] (:effect ctx)]
    (if (-> nexus :nexus/effects effect-k meta :nexus/batch)
      (-> (dissoc ctx :queue :stack)
          (update ::batched conjv (:effect ctx)))
      ctx)))

(defn after-dispatch [nexus ctx]
  (reduce
   (fn [ctx batch]
     (merge ctx (-> (execute-batch nexus ctx (ffirst batch) batch :effects wrap-batched-effect-handler)
                    (select-keys [:errors :results]))))
   (dissoc ctx ::batched)
   (batch-by first (::batched ctx))))

(defn install [nexus]
  (update nexus :nexus/interceptors (fnil conj [])
          {:id :batching
           :before-effect #(before-effect nexus %)
           :after-dispatch #(after-dispatch nexus %)}))
