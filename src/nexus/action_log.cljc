(ns nexus.action-log
  (:require [dataspex.core :as dataspex]
            [nexus.inspector :as inspector]
            [nexus.registry :as nxr]))

(defn create-log [& [opts]]
  (atom
   (-> (into {} opts)
       (update :slow-threshold #(or % 100)))))

(defn get-render-interceptor [log {:keys [label ns-aliases]}]
  {:after-dispatch
   (fn [ctx]
     (prn @log)
     (dataspex/inspect (or label "Actions")
       (inspector/->LogInspector @log)
       (cond-> {:track-changes? false
                :auditable? false}
         ns-aliases (assoc :ns-aliases ns-aliases)))
     ctx)})

(defn install-logger [nexus log & [opt]]
  (update nexus :nexus/interceptors
          (fn [interceptors]
            (-> (vec interceptors)
                (conj (get-render-interceptor log opt))
                (conj (inspector/get-interceptor log))))))

(defn ^{:deprecated "2026.06.1"} install-inspector [_log & [_opt]]
  )

(defn ^:export inspect
  {:arglists '[[]
               [{:keys [label ns-aliases]}]]}
  [& [opt]]
  (let [log (create-log)]
    (nxr/register-interceptor! (inspector/get-interceptor log))
    (nxr/register-interceptor! (get-render-interceptor log opt))))
