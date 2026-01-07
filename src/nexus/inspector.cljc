(ns nexus.inspector
  (:require [dataspex.data :as data]
            [dataspex.hiccup :as hiccup]
            [dataspex.protocols :as dp]
            [dataspex.time :as time]
            [dataspex.ui :as-alias ui]
            [dataspex.views :as views]))

(defn render-action [action opt]
  (hiccup/render-source
   action
   (assoc opt
          :dataspex/hiccup? false
          ::ui/line-length 100)))

(defn render-actions [actions opt]
  (into [:div] (map #(render-action % opt) actions)))

(defrecord Action [data f]
  dp/IRenderInline
  (render-inline [_ opt]
    (f data opt))

  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (f data opt))

  dp/IRenderSource
  (render-source [_ opt]
    (hiccup/render-source data (assoc opt :dataspex/hiccup? false)))

  dp/ICopy
  (copy-as-string [_]
    (data/stringify data))

  dp/IPrefersView
  (dp/get-preferred-view [_]
    views/source))

(defn ->action [action]
  (when action
    (->Action action render-action)))

(defn ->actions [actions]
  (when (seq actions)
    (->Action (vec actions) render-actions)))

(defn get-detail-entries [{:keys [dispatched-at actions dispatch-data
                                  effects results expansions dom-event
                                  dispatch-elapsed state error errors]}]
  (let [effects-action (->actions effects)]
    (concat
     [{:label (hiccup/string-label "Dispatched at")
       :k :dispatched-at
       :v dispatched-at}]
     (when dom-event
       [{:label (hiccup/string-label "DOM Event")
         :k :dom-event
         :v dom-event}])
     (if (or (nil? effects) (= (:data effects-action) (:data actions)))
       [{:label (hiccup/string-label "Actions")
         :k :dispatched
         :v actions}]
       (->> [{:label (hiccup/string-label "Actions")
              :k :dispatched
              :v actions}
             (when (seq effects)
               {:label (hiccup/string-label "Effects")
                :k :effects
                :v effects-action})
             {:label (hiccup/string-label "Expansions")
              :k :expansions
              :v expansions}]
            (remove nil?)))
     [{:label (hiccup/string-label "State")
       :k :state
       :v state}
      {:label (hiccup/string-label "Dispatch data")
       :k :dispatch-data
       :v dispatch-data}]
     (when error
       [{:label (hiccup/string-label "Error")
         :k :error
         :v error}])
     (when errors
       [{:label (hiccup/string-label "Errors")
         :k :errors
         :v errors}])
     [{:label (hiccup/string-label "Results")
       :k :results
       :v (keep :res results)}
      {:label (hiccup/string-label "Dispatch time")
       :k :dispatch-elapsed
       :v (hiccup/string-label (str dispatch-elapsed "ms"))}])))

(deftype ActionDetails [details]
  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (hiccup/render-entries-dictionary details opt (get-detail-entries details)))

  dp/IRenderSource
  (render-source [_ opt]
    (hiccup/render-source details opt))

  dp/ICopy
  (copy-as-string [_]
    (data/stringify (:dispatched details))))

(defrecord ActionKey [idx dispatched-at]
  dp/IRenderInline
  (render-inline [_ _]
    [::ui/code (time/hh:mm:ss dispatched-at)]))

(defn get-action-log [log]
  (->> log
       (map-indexed
        (fn [idx {:keys [dispatched-at actions]}]
          (let [k (->ActionKey idx dispatched-at)]
            {:label k
             :k k
             :v actions})))
       reverse))

(defn nav-in-log [log [k & ks]]
  (when-let [idx (when (instance? ActionKey k) (:idx k))]
    (if (empty? ks)
      (->ActionDetails (nth log idx))
      (-> (nth log idx)
          (data/nav-in ks)))))

(deftype LogInspector [log]
  dp/INavigatable
  (nav-in [_ path]
    (nav-in-log log path))

  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (hiccup/render-entries-dictionary log opt (get-action-log log)))

  dp/IRenderSource
  (render-source [_ opt]
    (hiccup/render-source log opt)))

(defn event? [x]
  (when x
    #?(:cljs (instance? js/Event x)
       :clj false)))

(defn now []
  #?(:cljs (js/Date.)
     :clj (java.util.Date.)))

(defn now-ms []
  #?(:cljs (.now js/performance)
     :clj  (/ (System/nanoTime) 1e6)))

(defn round-tenth [ms]
  (/ (Math/round (* ms 10)) 10.0))

(defn ^{:indent 1} update-entry [log idx f & args]
  (swap! log (fn [state]
               (apply update-in state [:entries idx] f args))))

(defn current-entry-idx [log]
  (peek (:entry-stack @log)))

(def conjv (fnil conj []))
(def intov (fnil into []))

(defn before-dispatch [log {:keys [dispatch-data state actions] :as ctx}]
  (let [event (->> (cond
                     (map? dispatch-data)
                     (vals dispatch-data)

                     (coll? dispatch-data)
                     dispatch-data

                     :else
                     nil)
                   (filter event?)
                   first)]
    (swap! log (fn [{:keys [entries] :as log-state}]
                 (let [entry-idx (count entries)]
                   (-> log-state
                       (update :entries conj
                               (cond-> {:dispatched-at (now)
                                        :actions (->actions actions)
                                        :dispatch-data dispatch-data
                                        :dispatch-start (now-ms)
                                        :state state}
                                 event (assoc :dom-event event)))
                       (update :entry-stack conj entry-idx)))))
    ctx))

(defn ->error [error]
  (cond-> error
    (:action error) (update :action ->action)
    (:effect error) (update :effect ->action)
    (:effects error) (update :effects ->actions)))

(defn after-dispatch [log ctx]
  (let [entry-idx (current-entry-idx log)]
    (swap! log update :entry-stack pop)
    (update-entry log entry-idx
      (fn [entry]
        (cond-> (-> entry
                    (assoc :dispatch-elapsed (round-tenth (- (now-ms) (:dispatch-start entry))))
                    (dissoc :dispatch-start)
                    (assoc :results (for [res (:results ctx)]
                                      (cond-> res
                                        (:effect res) (update :effect ->action)
                                        (:effects res) (update :effects ->actions)))))
          (= 1 (count (:errors ctx)))
          (assoc :error (->error (first (:errors ctx))))

          (< 1 (count (:errors ctx)))
          (assoc :errors (map ->error (:errors ctx)))))))
  ctx)

(defn after-action [log ctx]
  (update-entry log (current-entry-idx log)
    update :expansions conjv
    {:action    (->action (or (:nexus/action (meta (:action ctx)))
                              (:action ctx)))
     :expansion (->actions (:actions ctx))})
  ctx)

(defn before-effect [log ctx]
  (update-entry log (current-entry-idx log)
    (fn [entry]
      (if (:effects ctx)
        (-> entry
            (update :expansions intov
                    (->> (:effects ctx)
                         (filterv (comp :nexus/action meta))
                         (mapv (fn [effect]
                                 {:action (->action (:nexus/action (meta effect)))
                                  :expansion (->action effect)}))))
            (update :effects intov (:effects ctx)))
        (-> (cond-> entry
              (:nexus/action (meta (:effect ctx)))
              (update :expansions conjv
                      {:action (->action (:nexus/action (meta (:effect ctx))))
                       :expansion (->action (:effect ctx))}))
            (update :effects conjv (:effect ctx))))))
  ctx)

(defn get-interceptor [log]
  {:id ::inspector
   :before-dispatch #(before-dispatch log %)
   :after-dispatch #(after-dispatch log %)
   :after-action #(after-action log %)
   :before-effect #(before-effect log %)})
