(ns nexus.inspector-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [nexus.action-log :as action-log]
            [nexus.core :as nexus]
            [nexus.inspector :as inspector]))

(def nexus
  {:nexus/system->state deref

   :nexus/actions
   {:actions/inc
    (fn [_ n]
      [[:effects/save [:number] (inc n)]])}

   :nexus/effects
   {:effects/save
    (fn [_ store k v]
      (swap! store assoc k v))

    :effects/save-batch
    ^:nexus/batch
    (fn [_ store kvs]
      (swap! store #(reduce (fn [s [k v]] (assoc s k v)) % kvs)))}

   :nexus/placeholders
   {:secret/number
    (fn [{:keys [num]}]
      num)}})

(defn inspect [nexus]
  (let [log (action-log/create-log)]
    {:nexus (action-log/install-logger nexus log)
     :log log
     :store (atom {})}))

(defn datafy [data]
  (walk/postwalk
   (fn [x]
     (cond-> x
       (and (:data x) (:f x)) :data))
   (:entries data)))

(defn fake-clock []
  (let [now (atom 0)]
    (fn []
      (swap! now inc))))

(deftest logger-test
  (testing "Logs action at dispatch"
    (is (= (let [now (atom 0)]
             (with-redefs [inspector/now (constantly #inst "2025-06-13T08:40")
                           inspector/now-ms #(float (swap! now inc))]
               (let [{:keys [log nexus store]} (inspect nexus)]
                 (nexus/dispatch nexus store {:data 42} [[:actions/inc 2]])
                 (datafy @log))))
           [{:dispatched-at #inst "2025-06-13T08:40:00.000-00:00"
             :actions [[:actions/inc 2]]
             :dispatch-data {:data 42}
             :state {}
             :expansions [{:action [:actions/inc 2]
                           :expansion [[:effects/save [:number] 3]]}]
             :effects [[:effects/save [:number] 3]]
             :results [{:effect [:effects/save [:number] 3]
                        :res {[:number] 3}}]
             :dispatch-elapsed 1.0}])))

  (testing "Stores dispatches chronologically"
    (is (= (with-redefs [inspector/now (constantly #inst "2025-06-13T08:40")]
             (let [{:keys [log nexus store]} (inspect nexus)]
               (nexus/dispatch nexus store {:data 42} [[:actions/inc 2]])
               (nexus/dispatch nexus store {:data 42} [[:actions/inc 3]])
               (map :actions (datafy @log))))
           [[[:actions/inc 2]]
            [[:actions/inc 3]]])))

  (testing "Logs actions before and after placeholder interpolation"
    (is (= (let [{:keys [log nexus store]} (inspect nexus)]
             (nexus/dispatch nexus store {:num 42} [[:actions/inc [:secret/number]]])
             (:expansions (first (datafy @log))))
           [{:action [:actions/inc [:secret/number]]
             :expansion [[:effects/save [:number] 43]]}])))

  (testing "Logs dispatched effect"
    (is (= (let [{:keys [log nexus store]} (inspect nexus)]
             (nexus/dispatch nexus store {:num 42} [[:effects/save [:number] [:secret/number]]])
             (-> (datafy @log)
                 first
                 :expansions))
           [{:action [:effects/save [:number] [:secret/number]]
             :expansion [:effects/save [:number] 42]}])))

  (testing "Logs dispatched batched effect"
    (is (= (let [{:keys [log nexus store]} (inspect nexus)]
             (nexus/dispatch nexus store {:num 42}
               [[:effects/save-batch [:number] [:secret/number]]
                [:effects/save-batch [:name] "Nexus"]])
             (-> (datafy @log)
                 first
                 (select-keys [:actions :expansions])))
           {:actions [[:effects/save-batch [:number] [:secret/number]]
                      [:effects/save-batch [:name] "Nexus"]]
            :expansions [{:action [:effects/save-batch [:number] [:secret/number]]
                          :expansion [:effects/save-batch [:number] 42]}]}))))

(deftest nested-dispatch-corrupts-inspector-log-test
  (testing "Inspector handles effects that trigger nested dispatch"
    ;; Bug: when an effect triggers a nested dispatch, subsequent effects from
    ;; the original dispatch are logged incorrectly.
    ;;
    ;; Timeline:
    ;; 1. Original dispatch starts, before-dispatch creates log entry at index 0
    ;; 2. First effect runs and calls (:dispatch ctx) triggering nested dispatch
    ;; 3. Nested dispatch creates log entry at index 1
    ;; 4. Nested dispatch completes, after-dispatch converts entry 1's :effects
    ;;    from a vector to an Action record
    ;; 5. Original dispatch continues with its remaining effects
    ;; 6. before-effect calls update-current which updates entry at
    ;;    (dec (count log)) = 1, the WRONG entry
    ;; 7. conjv on Action record treats effect vector as map entry [key value]
    ;;
    ;; Symptoms:
    ;; - 2+ element effects: silently added as map entry to Action record
    ;; - 1-element effects: throws IndexOutOfBoundsException (in ClojureScript)
    (let [test-nexus {:nexus/system->state deref
                      :nexus/effects
                      {:effect/trigger-nested
                       (fn [{:keys [dispatch]} _store]
                         (dispatch [[:effect/inner]]))
                       :effect/inner (fn [_ _store])
                       :effect/outer (fn [_ _store _arg])}}
          log (action-log/create-log)
          nexus-with-log (action-log/install-logger test-nexus log)
          store (atom {})]

      (nexus/dispatch nexus-with-log store {}
                      [[:effect/trigger-nested]
                       [:effect/outer :arg1]])

      (let [[first-entry second-entry] (:entries @log)
            get-effects (fn [entry]
                          (let [effects (:effects entry)]
                            (if (and (map? effects) (:data effects))
                              (:data effects)
                              effects)))]
        ;; First entry (original dispatch) should have both effects
        (is (= [[:effect/trigger-nested] [:effect/outer :arg1]]
               (get-effects first-entry))
            "Original dispatch should log both of its effects")

        ;; Second entry (nested dispatch) should only have the inner effect
        (is (= [[:effect/inner]]
               (get-effects second-entry))
            "Nested dispatch should only log its own effect")

        ;; The nested entry's effects should not have extra keys from outer effects
        (let [second-effects (:effects second-entry)]
          (when (and (map? second-effects) (:data second-effects))
            (is (= #{:data :f} (set (keys second-effects)))
                "Nested dispatch's Action record should not have extra keys")))))))
