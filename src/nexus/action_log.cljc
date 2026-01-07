(ns nexus.action-log
  (:require [dataspex.core :as dataspex]
            [nexus.inspector :as inspector]
            [nexus.registry :as nxr]))

(defn create-log []
  (atom {:entries     []
         :entry-stack []}))

(defn install-logger [nexus log]
  (-> nexus
      (update :nexus/interceptors vec)
      (update :nexus/interceptors conj (inspector/get-interceptor log))))

(defn install-inspector [log & [{:keys [label ns-aliases]}]]
  (add-watch
   log ::inspect
   (fn [_ _ _ the-log]
     (when (contains? (last (:entries the-log)) :results)
       (dataspex/inspect (or label "Actions")
         (inspector/->LogInspector (:entries the-log))
         (cond-> {:track-changes? false}
           ns-aliases (assoc :ns-aliases ns-aliases)))))))

(defn ^:export inspect
  {:arglists '[[]
               [{:keys [label ns-aliases]}]]}
  [& [opt]]
  (let [log (create-log)]
    (nxr/register-interceptor! (inspector/get-interceptor log))
    (install-inspector log opt)))
