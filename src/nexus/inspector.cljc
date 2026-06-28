(ns nexus.inspector
  (:require [clojure.core.protocols :as p]
            [clojure.string :as str]
            [dataspex.data :as data]
            [dataspex.hiccup :as hiccup]
            [dataspex.icons :as-alias icons]
            [dataspex.protocols :as dp]
            [dataspex.time :as time]
            [dataspex.ui :as-alias ui]
            [dataspex.views :as views]))

(defn no-hiccup [opt]
  (assoc opt :dataspex/hiccup? false))

(defn render-action [action opt]
  (if (hiccup/summarize? action {:dataspex/summarize-above-w 200})
    [::ui/source
     (into [::ui/vector
            (hiccup/render-inline (first action) opt)]
           (for [arg (rest action)]
             (hiccup/render-inline arg (no-hiccup opt))))]
    (hiccup/render-source
     action
     (assoc opt
            :dataspex/hiccup? false
            ::ui/line-length 100))))

(defn render-actions [actions opt]
  (into [:div] (mapv #(render-action % opt) actions)))

(defrecord Action [action]
  dp/IRenderInline
  (render-inline [_ opt]
    (render-action action opt))

  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (render-action action opt))

  dp/IRenderSource
  (render-source [_ opt]
    (hiccup/render-source action (no-hiccup opt)))

  dp/ICopy
  (copy-as-string [_]
    (data/stringify action))

  dp/IPrefersView
  (dp/get-preferred-view [_]
    views/source))

(defrecord Actions [actions]
  dp/IRenderInline
  (render-inline [_ opt]
    (render-actions actions opt)))

(defn render-action-list [actions opt]
  (into [::ui/ul]
        (map-indexed
         (fn [idx action]
           [::ui/link
            {::ui/actions (->> (views/path-to opt [idx])
                               (views/navigate-to opt))}
            (render-action action opt)])
         actions)))

(defrecord ActionIndex [actions]
  dp/IRenderInline
  (render-inline [_ opt]
    (render-action-list (map :action actions) (update opt :dataspex/path conj :actions))))

(defrecord Timing [ms slow?]
  dp/IRenderInline
  (render-inline [_ _]
    (cond->> (str ms "ms")
      slow? (conj [::ui/info {:style {:color "var(--error-fg)"}}
                   [::icons/clock]]))))

(defn round-tenth [ms]
  (/ (Math/round (double (* ms 10))) 10.0))

(defn ->timing [ms & [{:keys [slow?]}]]
  (->Timing (round-tenth ms) (boolean slow?)))

(defn measure-elapsed [{:keys [slow-threshold]} now-ms then-ms]
  (let [ms (- now-ms then-ms)]
    (->timing ms {:slow? (<= slow-threshold ms)})))

(defn just-effects? [actions]
  (every? (fn [{:keys [action effects]}]
            (= [action] effects)) actions))

(declare get-action-entries)
(declare ->ActionDetail)

(defrecord ActionDetail [dispatched-action]
  dp/IRenderInline
  (render-inline [_ opt]
    (or (some-> (or (:action dispatched-action)
                    (:effect dispatched-action))
                (render-action opt))
        (render-actions (:effects dispatched-action) opt)))

  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (->> (get-action-entries dispatched-action)
         (hiccup/render-entries-dictionary dispatched-action (no-hiccup opt))))

  dp/IKeyLookup
  (lookup [_ _]
    dispatched-action)

  dp/INavigatable
  (nav-in [_ [k & ks]]
    (cond-> (if (keyword? k)
              (get dispatched-action k)
              (->ActionDetail (:dispatched-action k)))
      ks (data/nav-in ks))))

(defrecord ActionKey [dispatched-action]
  dp/IRenderInline
  (render-inline [_ opt]
    (-> (or (:action dispatched-action)
            (:effect dispatched-action)
            (first (:effects dispatched-action)))
        first
        (hiccup/render-inline (no-hiccup opt))))

  dp/IKeyLookup
  (lookup [_ _]
    (->ActionDetail dispatched-action)))

(defn get-error-entries [{:keys [effect effect-k err phase trace action]}]
  (concat
   (when phase
     [{:label (hiccup/string-label "Phase")
       :k :phase
       :v phase}])
   (when effect
     [{:label (hiccup/string-label "Effect")
       :k :effect
       :v (->Action effect)}])
   (when effect-k
     [{:label (hiccup/string-label "Effect kind")
       :k :effect-k
       :v effect-k}])
   (when action
     [{:label (hiccup/string-label "Action")
       :k :action
       :v (->Action action)}])
   (when trace
     (map-indexed
      (fn [idx action]
        (cond-> {:v (->Action action)}
          (= 0 idx) (assoc :label (hiccup/string-label "Trace"))))
      trace))
   (when err
     [{:label (hiccup/string-label "Error")
       :k :err
       :v err}])))

(defrecord ErrorDetail [error]
  dp/IRenderInline
  (render-inline [_ opt]
    [:div
     (:phase error) " "
     (hiccup/render-inline (:err error) opt)])

  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (->> (get-error-entries error)
         (hiccup/render-entries-dictionary error (no-hiccup opt))))

  dp/INavigatable
  (nav-in [_ [k & ks]]
    (cond-> (if k
              (get error k)
              error)
      ks (data/nav-in ks))))

(defrecord ErrorKey [error]
  dp/IRenderInline
  (render-inline [_ opt]
    [::ui/info
     [::icons/warning {:style {:color "var(--error-fg)"}}]
     (hiccup/render-inline (or (first (or (:effect error)
                                          (:action error)))
                               (:effect-k error)) opt)])

  dp/IKeyLookup
  (lookup [_ _]
    (->ErrorDetail error)))

(defn get-action-entries [{:keys [action interpolated interpolations interpolation-elapsed
                                  state expansions expansion-elapsed effect effects effect-elapsed result]}]
  (concat
   (when action
     [{:label (hiccup/string-label "Action")
       :k :action
       :v (->Action action)}])
   (when effect
     [{:label (hiccup/string-label "Effect")
       :k :effect
       :v (->Action effect)}])
   (when effects
     [{:label (hiccup/string-label "Effects")
       :k :effects
       :v (->Actions effects)}])
   (when interpolations
     [{:label (hiccup/string-label "Interpolations")
       :k :interpolations
       :v interpolations}
      {:label (hiccup/string-label "Interpolated")
       :k :interpolated
       :v (->Action interpolated)}
      {:label (hiccup/string-label "Interpolation elapsed")
       :k :interpolation-elapsed
       :v interpolation-elapsed}])
   [{:label (hiccup/string-label "State")
     :k :state
     :v state}]
   (when (seq (filter :action expansions))
     (->> expansions
          (mapv
           (fn [expansion]
             (when (:action expansion)
               {:k (->ActionKey expansion)
                :v (->ActionDetail expansion)})))
          (filterv second)
          (mapv (fn [idx entry]
                  (cond-> entry
                    (= 0 idx) (assoc :label (hiccup/string-label "Expanded actions"))))
                (range))))
   (when expansion-elapsed
     [{:label (hiccup/string-label "Expansion elapsed")
       :k :expansion-elapsed
       :v expansion-elapsed}])
   (when (seq (filter :effect expansions))
     (->> expansions
          (mapv
           (fn [expansion]
             (when (:effect expansion)
               {:k (->ActionKey expansion)
                :v (->ActionDetail expansion)})))
          (filterv second)
          (mapv (fn [idx entry]
                  (cond-> entry
                    (= 0 idx) (assoc :label (hiccup/string-label "Effects"))))
                (range))))
   (when effect-elapsed
     [{:label (hiccup/string-label "Result")
       :k :result
       :v result}
      {:label (hiccup/string-label "Effect elapsed")
       :k :effect-elapsed
       :v effect-elapsed}])))

(declare ->Dispatch)

(deftype DispatchId [id dispatched-at]
  dp/IRenderInline
  (render-inline [_ _]
    [::ui/code (time/hh:mm:ss dispatched-at)])

  dp/IKeyLookup
  (lookup [_ log]
    (->Dispatch
     (first (filter (comp #{id} :id) log)))))

(defn get-dispatch-entries [{:keys [dispatched-at dispatched-by dispatch-data dom-event
                                    actions effects error errors dispatch-elapsed]}]
  (concat
   [{:label (hiccup/string-label "Dispatched at")
     :k :dispatched-at
     :v dispatched-at}]
   (when dispatched-by
     [{:label (hiccup/string-label "Dispatched by")
       :absolute-path [(->DispatchId (:id dispatched-by) (:dispatched-at dispatched-by))]
       :v (->Actions (:actions dispatched-by))}])
   (when dom-event
     [{:label (hiccup/string-label "DOM Event")
       :k :dom-event
       :v dom-event}])
   [{:label (hiccup/string-label "Dispatch data")
     :k :dispatch-data
     :v dispatch-data}]
   (when-not (just-effects? actions)
     (map-indexed
      (fn [idx dispatched-action]
        (cond-> {:k (->ActionKey dispatched-action)
                 :v (->ActionDetail dispatched-action)}
          (= 0 idx) (assoc :label (hiccup/string-label "Actions"))))
      actions))
   (map-indexed
    (fn [idx effect-expansion]
      (cond-> {:k (->ActionKey effect-expansion)
               :v (->ActionDetail effect-expansion)}
        (= 0 idx) (assoc :label (hiccup/string-label "Effects"))))
    effects)
   (when error
     [{:label (hiccup/string-label "Error")
       :k :error
       :v error}])
   (when errors
     (map-indexed
      (fn [idx error]
        (cond-> {:k (->ErrorKey error)
                 :v (->ErrorDetail error)}
          (= 0 idx) (assoc :label (hiccup/string-label "Errors"))))
      errors))
   (let [dispatches (mapcat :dispatches effects)]
     (->> dispatches
          (map-indexed
           (fn [idx {:keys [id dispatched-at actions]}]
             (cond-> {:absolute-path [(->DispatchId id dispatched-at)]
                      :v (->Actions actions)}
               (= 0 idx) (assoc :label (hiccup/string-label
                                        (if (= 1 (count dispatches))
                                          "Nested dispatch"
                                          "Nested dispatches"))))))))
   [{:label (hiccup/string-label "Dispatch elapsed")
     :k :dispatch-elapsed
     :v dispatch-elapsed}]))

(deftype Dispatch [dispatch]
  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (hiccup/render-entries-dictionary dispatch opt (get-dispatch-entries dispatch)))

  dp/IRenderSource
  (render-source [_ opt]
    (hiccup/render-source dispatch opt))

  dp/ICopy
  (copy-as-string [_]
    (data/stringify (mapv :action (:actions dispatch))))

  p/Datafiable
  (datafy [_]
    dispatch))

(defn render-inline-dispatch [{:keys [dispatched-at dispatch-elapsed errors]} level]
  [::ui/info
   (when (< 1 level)
     (str/join (repeat (dec level) " ")))
   (when (< 0 level)
     [::icons/arrow-bend-down-right])
   [::ui/code (time/hh:mm:ss dispatched-at)]
   (when (:slow? dispatch-elapsed)
     [::icons/clock {:style {:color "var(--error-fg)"}}])
   (when (seq errors)
     [::icons/warning {:style {:color "var(--error-fg)"}}])])

(defrecord DispatchLabel [dispatch level]
  dp/IRenderInline
  (render-inline [_ _]
    (render-inline-dispatch dispatch level)))

(defrecord DispatchKey [id dispatched-at]
  dp/IRenderInline
  (render-inline [_ _]
    [::ui/code (time/hh:mm:ss dispatched-at)])

  dp/IKeyLookup
  (lookup [_ log]
    (->Dispatch (get (:entries log) id))))

(defn ->dispatch-key [dispatch]
  (->DispatchKey (:id dispatch) (:dispatched-at dispatch)))

(defn render-dispatch [dispatch entries level]
  (let [k (->dispatch-key dispatch)]
    (cons {:label (->DispatchLabel dispatch level)
           :k k
           :v (->> (:actions dispatch)
                   (mapcat #(if-let [one (or (:action %) (:effect %))]
                              [one]
                              (:effects %)))
                   ->Actions)}
          (->> (:dispatches dispatch)
               (map entries)
               (mapcat #(render-dispatch % entries (inc level)))))))

(defn ->ms [{:keys [seconds minutes hours days]}]
  (when (or seconds minutes hours days)
    (* (+ (or seconds 0)
          (* (or minutes 0) 60)
          (* (or hours 0) 60 60)
          (* (or days 0) 60 60 24))
       1000)))

(defn get-filtered-entries [{:keys [chronology entries max-entries max-age now]}]
  (let [since (some->> (->ms max-age)
                       (- (.getTime now)))]
    (cond->> chronology
      max-entries (take max-entries)
      since (take-while #(<= since (.getTime (get-in entries [% :dispatched-at])))))))

(defn get-action-log [{:keys [entries] :as opt}]
  (->> (get-filtered-entries opt)
       (map entries)
       (remove :dispatched-by)
       (mapcat #(render-dispatch % entries 0))))

(deftype LogInspector [log]
  dp/IRenderDictionary
  (render-dictionary [_ opt]
    (hiccup/render-entries-dictionary log opt (get-action-log log)))

  dp/IRenderSource
  (render-source [_ opt]
    (hiccup/render-source
     (mapv (:entries log) (:chronology log))
     opt))

  p/Datafiable
  (datafy [_]
    log))

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

(defn find-event [dispatch-data]
  (->> (cond
         (map? dispatch-data)
         (vals dispatch-data)

         (coll? dispatch-data)
         dispatch-data)
       (filter event?)
       first))

(defn get-log-idx [log ctx]
  (.indexOf (mapv :id @log) (::id ctx)))

(defn ^{:indent 2} update-log-entry [log ctx f & args]
  (apply swap! log update-in [:entries (::id ctx)] f args))

(def conjv (fnil conj []))

(defn get-dispatch-stub [log id]
  (let [dispatch (get-in @log [:entries id])]
    (assoc (select-keys dispatch [:id :dispatched-at])
           :actions (mapv (fn [{:keys [action effect]}]
                            (or action effect)) (:actions dispatch)))))

(defn before-dispatch [log {:keys [dispatch-data] :as ctx}]
  (let [id (random-uuid)
        event (find-event dispatch-data)
        ms (now-ms)]
    (swap! log
           #(cond-> (assoc-in % [:entries id]
                              (cond-> {:id id
                                       :dispatched-at (now)
                                       :actions []
                                       :dispatch-data dispatch-data}
                                event (assoc :dom-event event)
                                (::id ctx) (assoc :dispatched-by (get-dispatch-stub log (::id ctx)))))
              (::id ctx) (update-in [:entries (::id ctx) :dispatches] conjv id)
              :then (update :chronology conj id)))
    (assoc ctx
           ::id id
           ::dispatch-start ms
           ::path [:actions]
           ::before-interpolate ms
           ::dispatched-actions (:actions ctx)
           ::parent-path (when (::id ctx)
                           (concat [:entries (::id ctx)] (::path ctx) [:dispatches])))))

(defn after-dispatch [log ctx]
  (update-log-entry log ctx
    (fn [entry]
      (cond-> (assoc entry :dispatch-elapsed
                     (measure-elapsed @log (now-ms) (::dispatch-start ctx)))
        (and (empty? (:actions entry))
             (::dispatched-actions ctx)
             (:errors ctx))
        (assoc :actions (for [action (::dispatched-actions ctx)]
                          {:action action}))

        (:errors ctx)
        (assoc :errors (:errors ctx)))))
  (when-let [path (::parent-path ctx)]
    (swap! log update-in path conjv (get-dispatch-stub log (::id ctx))))
  ctx)

(defn before-action [log ctx]
  (let [idx (-> (get log (get-log-idx log ctx))
                (get-in (::path ctx))
                count)
        now (now-ms)]
    (update-log-entry log ctx
      (fn [entry]
        (update-in entry (::path ctx) conjv
                   (-> (if-let [details (meta (:action ctx))]
                         {:action (:nexus/action details)
                          :interpolated (:action ctx)
                          :interpolations (->> (:nexus/interpolations details)
                                               (map (juxt :placeholder :resolution))
                                               (into {}))
                          :interpolation-elapsed (measure-elapsed @log now (::before-interpolate ctx))}
                         {:action (:action ctx)})
                       (assoc :state (:state ctx))))))
    (-> (update ctx ::path into [idx :expansions])
        (assoc ::before-action now))))

(defn after-action [log ctx]
  (update-log-entry log ctx
    (fn [entry]
      (assoc-in entry (conj (pop (::path ctx)) :expansion-elapsed)
                (measure-elapsed @log (now-ms) (::before-action ctx)))))
  (update ctx ::path #(pop (pop %))))

(defn before-effect [log ctx]
  (update-log-entry log ctx
    (fn [entry]
      (update-in entry (::path ctx) conjv
                 (merge {:state (:state ctx)}
                        (select-keys ctx [:effect :effects])))))
  (-> (update ctx ::path conj (dec (count (-> (:entries @log)
                                              (get (::id ctx))
                                              (get-in (::path ctx))))))
      (assoc ::before-effectuate (now-ms))))

(defn after-effect [log ctx]
  (let [now (now-ms)]
    (update-log-entry log ctx
      (fn [entry]
        (let [entry (update-in entry (::path ctx) into
                               {:result (:res ctx)
                                :effect-elapsed (measure-elapsed @log now (::before-effectuate ctx))})]
          (update entry :effects conjv (get-in entry (::path ctx)))))))
  (update ctx ::path pop))

(defn get-interceptor [log]
  {:id ::inspector
   :before-dispatch #(before-dispatch log %)
   :after-dispatch #(after-dispatch log %)
   :before-action #(before-action log %)
   :after-action #(after-action log %)
   :before-effect #(before-effect log %)
   :after-effect #(after-effect log %)})
