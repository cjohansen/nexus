(ns nexus.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]
            [nexus.test-helper :as h]))

(deftest action?-test
  (testing "Detects action"
    (is (false? (nexus/action? :action)))
    (is (true? (nexus/action? [:action])))
    (is (false? (nexus/action? ["action"])))))

(deftest actions?-test
  (testing "Detects actions"
    (is (false? (nexus/actions? :action)))
    (is (false? (nexus/actions? [:action])))
    (is (false? (nexus/actions? [["action"]])))
    (is (true? (nexus/actions? [[:action]])))
    (is (true? (nexus/actions? [[:actions/doit "Now!"]])))
    (is (true? (nexus/actions? [[:actions/doit "Now!"]
                                [:actions/also "Do" :this]])))))

(def nexus-with-inc
  {:nexus/actions
   {:actions/inc
    (fn [state n]
      [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}})

(deftest expand-action-test
  (testing "Assumes effect without any expansions"
    (is (= (->> [:actions/test :it]
                (nexus/expand-action {} {} {})
                :effects)
           [[:actions/test :it]])))

  (testing "Expands action"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [_ arg]
                   [[:actions/store (str/upper-case arg)]])}}
               (nexus/expand-action {} {} [:actions/test "it"])
               :effects)
           [[:actions/store "IT"]])))

  (testing "Passes state to action handler"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [state arg]
                   [[:actions/store (:n (:config state)) arg]])}}
               (nexus/expand-action {:config {:n 2}} {} [:actions/test "it"])
               :effects)
           [[:actions/store 2 "it"]])))

  (testing "Returns error when action handler does not return collection of actions"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [{:keys [config]} arg]
                   [:actions/store (:n config) arg])}}
               (nexus/expand-action {:config {:n 2}} {} [:actions/test "it"])
               h/datafy-errors)
           {:errors
            [{:action [:actions/test "it"]
              :trace [[:actions/test "it"]]
              :phase :expand-action
              :err {:message ":actions/test should expand to a collection of actions"
                    :data {:res [:actions/store 2 "it"]
                           :action [:actions/test "it"]}}}]})))

  (testing "Expands action to empty list of effects"
    (is (empty? (-> {:nexus/actions
                     {:actions/test
                      (fn [_ _] [])}}
                    (nexus/expand-action {:config {:n 2}} {} [:actions/test "it"])
                    :effects))))

  (testing "Expands actions recursively"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ n]
                   [[:actions/plus n 1]])
                 :actions/plus
                 (fn [_ a b]
                   [[:actions/store "n" (+ a b)]])}}
               (nexus/expand-action {} {} [:actions/inc 2])
               :effects)
           [[:actions/store "n" 3]])))

  (testing "Expands only first action"
    (is (= (-> {:nexus/actions
                {:actions/inc-much
                 (fn [_ n]
                   [[:actions/plus n 1]
                    [:actions/plus n 2]])
                 :actions/plus
                 (fn [_ a b]
                   [[:actions/store "n" (+ a b)]])}}
               (nexus/expand-action {} {} [:actions/inc-much 2]))
           {:effects [[:actions/store "n" 3]]
            :actions [[:actions/plus 2 2]]
            :trace [[:actions/inc-much 2]
                    [:actions/plus 2 1]]})))

  (testing "Interpolates placeholders in expanded actions"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ n]
                   [[:actions/plus n [:placeholders/one]]])
                 :actions/plus
                 (fn [_ a b]
                   [[:actions/store "n" (+ a b)]])}
                :nexus/placeholders
                {:placeholders/one (fn [dispatch-data] (:one dispatch-data))}}
               (nexus/expand-action {} {:dispatch-data {:one 1}} [:actions/inc 2])
               :effects)
           [[:actions/store "n" 3]])))

  (testing "Returns errors from bad action handler"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))}}
               (nexus/expand-action {} {} [:actions/inc 2])
               h/datafy-errors)
           {:errors [{:phase :expand-action
                      :action [:actions/inc 2]
                      :trace [[:actions/inc 2]]
                      :err {:message "Boom!"
                            :data {}}}]})))

  (testing "Calls before-interceptor before action handler"
    (is (= (-> (h/with-interceptor nexus-with-inc :before-action
                 #(assoc-in % [:state :base-n] 2))
               (nexus/expand-action {} {} [:actions/inc 9])
               :effects)
           [[:effects/save [:number] 12]])))

  (testing "Calls after-interceptor after action handler"
    (is (= (let [log (atom [])]
             (-> (h/with-interceptor nexus-with-inc :after-action
                   (fn [context]
                     (swap! log conj {:in (:action context) :out (:actions context)})
                     context))
                 (nexus/expand-action {} {} [:actions/inc 9]))
             @log)
           [{:in [:actions/inc 9]
             :out [[:effects/save [:number] 10]]}])))

  (testing "Does not call interceptors for actions that have no handlers"
    (is (= (let [log (atom [])]
             (-> {:nexus/interceptors
                  [{:before-action
                    (fn [ctx]
                      (swap! log conj ctx))}]}
                 (nexus/expand-action {:state "Here"} {} [:actions/inc 2]))
             @log)
           [])))

  (testing "Returns error from before-action interceptor"
    (is (= (-> (h/with-interceptor nexus-with-inc :before-action
                 (fn [ctx]
                   (throw (ex-info "Boom!" {:ctx ctx}))))
               (nexus/expand-action {:state "Here"} {} [:actions/inc 2])
               h/datafy-errors)
           {:effects [[:effects/save [:number] 3]]
            :errors
            [{:phase :before-action
              :err
              {:message "Boom!"
               :data
               {:ctx
                {:state {:state "Here"}
                 :action [:actions/inc 2]
                 :queue [{:phase :expand-action
                          :before-action ::h/fn}]
                 :stack [{:before-action ::h/fn}]}}}
              :action [:actions/inc 2]
              :trace [[:actions/inc 2]]}]})))

  (testing "Returns error from after-action interceptor"
    (is (= (-> (h/with-interceptor nexus-with-inc :after-action
                 (fn [ctx]
                   (throw (ex-info "Boom!" {:ctx ctx})))
                 :logger)
               (nexus/expand-action {:state "Here"} {} [:actions/inc 2])
               h/datafy-errors)
           {:effects [[:effects/save [:number] 3]]
            :errors
            [{:phase :after-action
              :id :logger
              :err
              {:message "Boom!"
               :data
               {:ctx
                {:state {:state "Here"}
                 :action [:actions/inc 2]
                 :queue nil
                 :stack nil
                 :actions [[:effects/save [:number] 3]]}}}
              :action [:actions/inc 2]
              :trace [[:actions/inc 2]]}]})))

  (testing "Allows interceptor to abort action expansion flow"
    (is (= (-> (h/with-interceptor nexus-with-inc :before-action
                 #(throw (ex-info "Boom!" {:ctx %}))
                 :logger)
               (h/with-interceptor :before-action
                   #(cond-> %
                      (:errors %) (dissoc :queue :stack))
                 :abort-early)
               (nexus/expand-action {:state "Here"} {} [:actions/inc 2])
               h/datafy-errors
               (update-in [:errors 0 :err] select-keys [:message]))
           ;; No effects!
           {:errors
            [{:id :logger
              :phase :before-action
              :action [:actions/inc 2]
              :trace [[:actions/inc 2]]
              :err {:message "Boom!"}}]})))

  (testing "Calls interceptors in order"
    (is (= (let [log (atom [])]
             (-> {:nexus/actions
                  {:actions/inc
                   (fn [state n]
                     (swap! log conj [:action])
                     [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}
                  :nexus/interceptors [(h/log-interceptor log 1)
                                       (h/log-interceptor log 2)
                                       (h/log-interceptor log 3)]}
                 (nexus/expand-action {} {} [:actions/inc 9]))
             @log)
           [[:before-action 1 :actions/inc]
            [:before-action 2 :actions/inc]
            [:before-action 3 :actions/inc]
            [:action]
            [:after-action 3 :actions/inc [:effects/save]]
            [:after-action 2 :actions/inc [:effects/save]]
            [:after-action 1 :actions/inc [:effects/save]]])))


  (testing "Preserves order of residual actions during recursive expansion"
    ;; A repro / regression test for the bug addressed by https://github.com/cjohansen/nexus/pull/11
    ;; When recursively expanding actions, action order was being reversed
    (is (= (-> {:nexus/actions
                {:actions/outer (fn [_] [[:actions/inner] [:effects/tail]])
                 :actions/inner (fn [_] [[:effects/a] [:effects/b] [:effects/c]])}}
               (nexus/expand-action {} {} [:actions/outer])
               :actions
               vec)
           [[:effects/b] [:effects/c] [:effects/tail]]))))

(deftest interpolate-test
  (testing "No-ops when there are no placeholders"
    (is (= (nexus/interpolate {} {} [[:actions/inc 3]])
           [[:actions/inc 3]])))

  (testing "Replaces placeholder form"
    (is (= (-> {:nexus/placeholders {:number (fn [{:keys [value]}] value)}}
               (nexus/interpolate {:value 3} [[:actions/inc [:number]]]))
           [[:actions/inc 3]])))

  (testing "Does not replace naked placeholder keyword"
    (is (= (-> {:nexus/placeholders {:number (fn [{:keys [value]}] value)}}
               (nexus/interpolate {:value 3} [[:actions/inc :number]]))
           [[:actions/inc :number]])))

  (testing "Nests placeholders"
    (is (= (-> {:nexus/placeholders
                {:value (fn [{:keys [value]}] value)
                 :number (fn [_ s] (some-> s parse-long))}}
               (nexus/interpolate {:value "5"} [[:actions/inc [:number [:value]]]]))
           [[:actions/inc 5]])))

  (testing "Stores original action as meta data"
    (is (= (-> {:nexus/placeholders
                {:value (fn [{:keys [value]}] value)
                 :number (fn [_ s] (some-> s parse-long))}}
               (nexus/interpolate {:value "5"} [[:actions/inc [:number [:value]]]])
               first
               meta
               :nexus/action)
           [:actions/inc [:number [:value]]])))

  (testing "Does not store meta on actions without interpolations"
    (is (nil? (-> {}
                  (nexus/interpolate {:value "5"} [[:actions/inc 3]])
                  first
                  meta
                  :nexus/action))))

  (testing "Stores placeholder->resolution as meta data"
    (is (= (-> {:nexus/placeholders
                {:value (fn [{:keys [value]}] value)
                 :number (fn [_ s] (some-> s parse-long))}}
               (nexus/interpolate {:value "5"} [[:actions/inc [:number [:value]]]])
               first
               meta
               :nexus/interpolations)
           [{:placeholder [:value]
             :resolution "5"}
            {:placeholder [:number "5"]
             :resolution 5}])))

  (testing "Can opt out of interpolation with meta data"
    (is (= (-> {:nexus/placeholders {:number (fn [{:keys [value]}] value)}}
               (nexus/interpolate {:value 3} [[:actions/inc ^:nexus/skip-interpolation [:number]]]))
           [[:actions/inc [:number]]]))))

(def nexus-with-save
  {:nexus/effects
   {:effects/save
    (fn [_ system path v]
      (swap! system assoc-in path v))}})

(def nexus-with-batched-save
  {:nexus/system->state deref
   :nexus/effects
   {:effects/save
    ^:nexus/batch
    (fn [_ system path-vs]
      (swap! system
             (fn [state]
               (reduce (fn [s [p v]] (assoc-in s p v)) state path-vs))))}})

(deftest execute-test
  (testing "Fails when there is no implementation"
    (is (= (-> (nexus/execute {} {:system (atom {})} [:effects/save [:number] 3])
               h/datafy-errors)
           {:errors [{:phase :execute-effect
                      :effect-k :effects/save
                      :err {:message "No such effect"
                            :data {:available-effects nil}}}]})))

  (testing "Returns result of executing effect"
    (is (= (-> {:nexus/effects
                {:effects/save
                 (fn [_ system path v]
                   (swap! system assoc-in path v))}}
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [:effects/save [:number] 3])
               :results)
           [{:effect [:effects/save [:number] 3]
             :res {:existing "Data"
                   :number 3}}])))

  (testing "Calls interceptors in order"
    (is (= (let [log (atom [])]
             (-> nexus-with-save
                 (assoc :nexus/interceptors
                        [(h/log-interceptor log 1)
                         (h/log-interceptor log 2)
                         (h/log-interceptor log 3)])
                 (nexus/execute
                  {:system (atom {:existing "Data"})}
                  [:effects/save [:number] 9]))
             @log)
           [[:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:existing "Data", :number 9}]
            [:after-effect 2 :effects/save {:existing "Data", :number 9}]
            [:after-effect 1 :effects/save {:existing "Data", :number 9}]]))))

(deftest dispatch-handler-test
  (testing "Expands action and executes effect"
    (is (= (-> nexus-with-save
               (nexus/dispatch-handler
                (constantly nil)
                {:system (atom {:existing "Data"})
                 :actions [[:effects/save [:number] 3]
                           [:effects/save [:name] "Nexus"]]})
               :results)
           [{:effect [:effects/save [:number] 3]
             :res {:existing "Data"
                   :number 3}}
            {:effect [:effects/save [:name] "Nexus"]
             :res {:existing "Data"
                   :number 3
                   :name "Nexus"}}])))

  (testing "Processes effects one by one"
    (is (= (-> nexus-with-save
               (nexus/dispatch-handler
                (constantly nil)
                {:system (atom {:existing "Data"})
                 :actions [[:effects/save [:number] 3]
                           [:effects/save [:name] "Nexus"]]})
               :results)
           [{:effect [:effects/save [:number] 3]
             :res {:existing "Data"
                   :number 3}}
            {:effect [:effects/save [:name] "Nexus"]
             :res {:existing "Data"
                   :number 3
                   :name "Nexus"}}]))))

(deftest dispatch-test
  (testing "Expands, interpolates and executes action"
    (is (= (let [system (atom {:step-size 3})]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dispatch/number (fn [{:keys [value]}] value)}}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch system {:value 5} [[:actions/inc [:dispatch/number]]]))
             @system)
           {:step-size 3
            :number 6})))

  (testing "Returns results"
    (is (= (let [system (atom {:step-size 3})]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dispatch/number (fn [{:keys [value]}] value)}}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch system {:value 5} [[:actions/inc [:dispatch/number]]])
                 :results))
           [{:effect [:effects/save [:number] 6]
             :res {:step-size 3
                   :number 6}}])))

  (testing "Returns errors from effect"
    (is (= (-> {:nexus/system->state deref
                :nexus/effects
                {:effects/fail
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))}}
               (nexus/dispatch (atom nil) {} [[:effects/fail]])
               h/datafy-errors)
           {:errors [{:effect [:effects/fail]
                      :err {:data {}
                            :message "Boom!"}
                      :phase :execute-effect
                      :trace [[:effects/fail]]}]})))

  (testing "Includes action in effect error trace"
    (is (= (-> {:nexus/system->state deref
                :nexus/actions
                {:actions/prepare-to-fail
                 (fn [_]
                   [[:effects/fail]])}
                :nexus/effects
                {:effects/fail
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))}}
               (nexus/dispatch (atom nil) {} [[:actions/prepare-to-fail]])
               h/datafy-errors)
           {:errors [{:effect [:effects/fail]
                      :err {:data {}
                            :message "Boom!"}
                      :phase :execute-effect
                      :trace [[:actions/prepare-to-fail]
                              [:effects/fail]]}]})))

  (testing "Does not return errors from effect that is dispatched from effect"
    ;; The nested dispatch is intended for async usage. Re-emitting errors from
    ;; this function only works for synchronous calls, and in that case any
    ;; user-provided interceptors will see the same error twice. Nexus used to
    ;; re-emit these errors, but doing so was a mistake.
    (is (= (-> {:nexus/system->state deref
                :nexus/effects
                {:effects/fail
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))
                 :effects/dispatch-fail
                 (fn [{:keys [dispatch]} _]
                   (dispatch [[:effects/fail]]))}}
               (nexus/dispatch (atom nil) {} [[:effects/dispatch-fail]])
               h/datafy-errors)
           {:errors []})))

  (testing "Errors from nested dispatch can be intercepted"
    (is (= (let [intercepted-errors (atom nil)]
             (-> {:nexus/system->state deref
                  :nexus/interceptors [{:after-effect
                                        (fn [ctx]
                                          (swap! intercepted-errors into (:errors ctx))
                                          ctx)}]
                  :nexus/effects
                  {:effects/fail
                   (fn [_ _]
                     (throw (ex-info "Boom!" {})))
                   :effects/dispatch-fail
                   (fn [{:keys [dispatch]} _]
                     (dispatch [[:effects/fail]]))}}
                 (nexus/dispatch (atom nil) {} [[:effects/dispatch-fail]]))
             (h/datafy-errors* @intercepted-errors))
           [{:phase :execute-effect
             :err {:message "Boom!"
                   :data {}}
             :effect [:effects/fail]
             :trace [[:effects/dispatch-fail]
                     [:effects/fail]]}])))

  (testing "Tracks sources across multiple levels of dispatch"
    (is (= (let [intercepted-errors (atom nil)]
             (-> {:nexus/system->state deref
                  :nexus/interceptors [{:after-effect
                                        (fn [ctx]
                                          (swap! intercepted-errors into (:errors ctx))
                                          ctx)}]
                  :nexus/effects
                  {:effects/fail
                   (fn [_ _]
                     (throw (ex-info "Boom!" {})))

                   :effects/dispatch-fail
                   (fn [{:keys [dispatch]} _]
                     (dispatch [[:effects/fail]]))

                   :effects/prepare-to-fail
                   (fn [{:keys [dispatch]} _]
                     (dispatch [[:effects/dispatch-fail]]))}}
                 (nexus/dispatch (atom nil) {} [[:effects/prepare-to-fail]]))
             (h/datafy-errors* @intercepted-errors))
           [{:phase :execute-effect
             :err {:message "Boom!"
                   :data {}}
             :effect [:effects/fail]
             :trace [[:effects/prepare-to-fail]
                     [:effects/dispatch-fail]
                     [:effects/fail]]}])))

  (testing "Runs interceptors in order"
    (is (= (let [system (atom {:step-size 3})
                 log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dispatch/number (fn [{:keys [value]}] value)}
                  :nexus/interceptors [(h/log-interceptor log 1)
                                       (h/log-interceptor log 2)
                                       (h/log-interceptor log 3)]}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch system {:value 5} [[:actions/inc [:dispatch/number]]]))
             @log)
           [[:before-dispatch 1 [[:actions/inc [:dispatch/number]]]]
            [:before-dispatch 2 [[:actions/inc [:dispatch/number]]]]
            [:before-dispatch 3 [[:actions/inc [:dispatch/number]]]]
            [:before-action 1 :actions/inc]
            [:before-action 2 :actions/inc]
            [:before-action 3 :actions/inc]
            [:after-action 3 :actions/inc [:effects/save]]
            [:after-action 2 :actions/inc [:effects/save]]
            [:after-action 1 :actions/inc [:effects/save]]
            [:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:step-size 3, :number 6}]
            [:after-effect 2 :effects/save {:step-size 3, :number 6}]
            [:after-effect 1 :effects/save {:step-size 3, :number 6}]
            [:after-dispatch 3 [{:effect [:effects/save [:number] 6]
                                 :res {:step-size 3, :number 6}}]]
            [:after-dispatch 2 [{:effect [:effects/save [:number] 6]
                                 :res {:step-size 3, :number 6}}]]
            [:after-dispatch 1 [{:effect [:effects/save [:number] 6]
                                 :res {:step-size 3, :number 6}}]]])))

  (testing "Dispatches actions recursively with new dispatch data"
    (is (= (let [system (atom {:step-size 3})
                 log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dd/k (fn [dispatch-data k]
                                               (prn dispatch-data k)
                                               (if (contains? dispatch-data k)
                                                 (k dispatch-data)
                                                 [:dd/k k]))}
                  :nexus/interceptors [(h/log-interceptor log 1)]
                  :nexus/effects {:effects/save
                                  (fn [{:keys [dispatch]} system path v & [{:keys [on-success]}]]
                                    (let [res (swap! system assoc-in path v)]
                                      (when on-success
                                        (dispatch on-success res))
                                      res))}}
                 (merge nexus-with-inc)
                 (nexus/dispatch system {:value 5}
                                 [[:effects/save [:number] [:dd/k :value]
                                   {:on-success [[:effects/save [:second-number] [:dd/k :number]]]}]]))
             [@system @log])
           [{:step-size 3
             :number 5
             :second-number 5}
            [[:before-dispatch 1 [[:effects/save [:number] [:dd/k :value]
                                   {:on-success [[:effects/save [:second-number] [:dd/k :number]]]}]]]
             [:before-effect 1 :effects/save]
             [:before-dispatch 1 [[:effects/save [:second-number] [:dd/k :number]]]]
             [:before-effect 1 :effects/save]
             [:after-effect 1 :effects/save {:number 5, :second-number 5, :step-size 3}]
             [:after-dispatch 1 [{:effect [:effects/save [:second-number] 5], :res {:number 5, :second-number 5, :step-size 3}}]]
             [:after-effect 1 :effects/save {:number 5, :step-size 3}]
             [:after-dispatch 1 [{:effect [:effects/save [:number] 5
                                           {:on-success [[:effects/save [:second-number] [:dd/k :number]]]}],
                                  :res {:number 5, :step-size 3}}]]]])))

  (testing "Optionally batch processes effect"
    (is (= (-> nexus-with-batched-save
               (nexus/dispatch (atom {:existing "Data"}) {}
                   [[:effects/save [:number] 3]
                    [:effects/save [:name] "Nexus"]]))
           {:results
            [{:effects [[:effects/save [:number] 3]
                        [:effects/save [:name] "Nexus"]]
              :res {:existing "Data"
                    :number 3
                    :name "Nexus"}}]})))

  (testing "Executes effects as close to dispatch order as possible, starting with unbatched ones"
    (is (= (let [log (atom [])]
             (-> {:nexus/effects
                  {:effects/save ^:nexus/batch
                   (fn [_ _ effect-args]
                     (doseq [args effect-args]
                       (swap! log conj (into [:effects/save] args))))

                   :effects/transact ^:nexus/batch
                   (fn [_ _ effect-args]
                     (doseq [args effect-args]
                       (swap! log conj (into [:effects/transact] args))))

                   :effects/alert
                   (fn [_ _ text]
                     (swap! log conj [:effects/alert text]))}}
                 (nexus/dispatch (atom {}) {}
                     [[:effects/save :a 1]
                      [:effects/transact :A 1]
                      [:effects/alert "First"]
                      [:effects/save :b 2]
                      [:effects/alert "Second"]
                      [:effects/transact :B 2]]))
             @log)
           [[:effects/alert "First"]
            [:effects/alert "Second"]
            [:effects/save :a 1]
            [:effects/save :b 2]
            [:effects/transact :A 1]
            [:effects/transact :B 2]])))

  (testing "Interpolates actions in between expansions"
    (is (= (->> (let [log (atom [])]
                  (-> {:nexus/system->state deref

                       :nexus/placeholders
                       {:rnd/gen-id (fn [_] (random-uuid))}

                       :nexus/actions
                       {:actions/create-new-thing
                        (fn [_ title]
                          [[:actions/create-thing [:rnd/gen-id] title]])

                        :actions/create-thing
                        (fn [_ id title]
                          [[:actions/save-thing id title]
                           [:actions/record-meta id "Created"]])

                        :actions/save-thing
                        (fn [_ id title]
                          [[:effects/save id title]])

                        :actions/record-meta
                        (fn [_ id data]
                          [[:effects/save id data]])}

                       :nexus/effects
                       {:effects/save ^:nexus/batch
                        (fn [_ _ effect-args]
                          (doseq [args effect-args]
                            (swap! log conj (into [:effects/save] args))))}}
                      (nexus/dispatch (atom {}) {}
                          [[:actions/create-new-thing "A thing!"]]))
                  @log)
                (map second)
                set
                count)
           1)))

  (testing "Updates state snapshot after every effect"
    (is (= (-> {:nexus/system->state deref

                :nexus/actions
                {:actions/create-thing
                 (fn [state title]
                   [[:effects/save [:things (:id state)] title]
                    [:effects/save [:id] (inc (:id state))]])}

                :nexus/effects
                {:effects/save
                 (fn [_ system path v]
                   (swap! system assoc-in path v))}}

               (nexus/dispatch (atom {:id 0}) {}
                 [[:actions/create-thing "A thing!"]
                  [:actions/create-thing "Another thing!"]])
               :results
               last
               :res)
           {:id 2
            :things {0 "A thing!"
                     1 "Another thing!"}})))

  (testing "Passes state to effect handlers"
    ;; This isn't really by design or a part of the documented contract, but was
    ;; an accidental part of earlier Nexus releases, so is preserved for
    ;; backwards compatibility.
    (is (= (-> {:nexus/system->state deref
                :nexus/effects
                {:effects/doit
                 (fn [{:keys [state]} _]
                   state)}}
               (nexus/dispatch (atom {:state "State"}) {} [[:effects/doit]])
               :results)
           [{:effect [:effects/doit]
             :res {:state "State"}}])))

  (testing "Nested dispatch that expands to no actions should not go into an infinite loop"
    (is (= (-> {:nexus/system->state deref
                :nexus/actions {:actions/prepare-it (fn [_] [[:effects/doit]])
                                :actions/noop (fn [_] [])}
                :nexus/effects
                {:effects/doit
                 (fn [{:keys [dispatch]} _]
                   (dispatch [[:actions/noop]]))}}
               (nexus/dispatch (atom {:state "State"}) {} [[:actions/prepare-it]]))
           {:results [{:effect [:effects/doit], :res {}}]})))

  (testing "Can hijack the dispatch function with an interceptor"
    (is (= (let [system! (atom {})]
             (-> {:nexus/system->state deref
                  :nexus/interceptors
                  [{:before-effect
                    (fn [ctx]
                      (update ctx :dispatch
                              (fn [dispatch]
                                (fn [actions]
                                  (dispatch (conj actions [:effects/save [:log] "Sneaky!"]))))))}]

                  :nexus/effects
                  {:effects/create
                   (fn [{:keys [dispatch]} system path v]
                     (swap! system assoc-in path v)
                     (dispatch [[:effects/save (conj path :created?) true]]))

                   :effects/save
                   (fn [_ system path v]
                     (swap! system assoc-in path v))}}
                 (nexus/dispatch system! {} [[:effects/create [:person] {:name "Chris"}]]))
             @system!)
           {:person {:name "Chris"
                     :created? true}
            :log "Sneaky!"})))

  (testing "Executes effects expanded from actions in order"
    (is (= (let [log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/actions
                  {:actions/add-k
                   (fn [_ k]
                     [[:effects/save k nil]])

                   :actions/inc
                   (fn [state k]
                     [[:effects/save k (inc (or (get state k) 0))]])

                   :actions/add-ks
                   (fn [_ ks]
                     (for [k ks]
                       [:actions/add-k k]))}

                  :nexus/effects
                  {:effects/save
                   (fn [_ system k v]
                     (swap! system assoc-in k v))}

                  :nexus/interceptors [{:before-effect (fn [ctx]
                                                         (swap! log conj (second (:effect ctx)))
                                                         ctx)}]}
                 (nexus/dispatch (atom {}) {}
                     [[:actions/inc :a]
                      [:actions/add-ks [:b :c :d]]
                      [:actions/add-ks [:e :f :g]]
                      [:actions/inc :h]]))
             @log)
           [:a :b :c :d :e :f :g :h])))

  (testing "Can make persistent modifications to the ctx with interceptors"
    (is (= (let [system (atom {:step-size 3})
                 log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/interceptors [{:before-dispatch #(assoc % :dispatch-id "dispatch-1")
                                        :before-action #(assoc % :action-id "action-1")
                                        :before-effect #(assoc % :effect-id "effect-1")
                                        :after-dispatch #(do (swap! log conj (select-keys % [:dispatch-id :action-id :effect-id]))
                                                             %)}]}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch system {:value 5} [[:actions/inc 3]]))
             @log)
           [{:dispatch-id "dispatch-1"
             :action-id "action-1"
             :effect-id "effect-1"}]))))

(deftest system+dispatch-data->state-test
  (testing "Uses :nexus/system+dispatch-data->state to grab state snapshot"
    (is (= (let [state (atom nil)]
             (-> {:nexus/system->state deref

                  :nexus/system+dispatch-data->state
                  (fn [system dispatch-data]
                    (assoc @system :now (:now dispatch-data)))

                  :nexus/actions
                  {:actions/noop (constantly nil)}

                  :nexus/interceptors [{:before-action (fn [ctx]
                                                         (reset! state (:state ctx))
                                                         ctx)}]}
                 (nexus/dispatch (atom {:num 1}) {:now 1000} [[:actions/noop]]))
             @state)
           {:num 1
            :now 1000}))))

(deftest dispatch-order
  (testing "Executes effects prior to action expansion"
    (is (= (h/test-dispatch-order nexus/dispatch [[:fx1] [:fx2] [:ax1]])
           [[:exec-effect [:fx1]]
            [:exec-effect [:fx2]]
            [:expand-action [:ax1]]
            [:exec-effect [:fx1]]
            [:exec-effect [:fx2]]])))

  (testing "Expands actions lazily"
    (is (= (h/test-dispatch-order nexus/dispatch [[:ax1] [:ax2]])
           [[:expand-action [:ax1]]
            [:exec-effect [:fx1]]
            [:exec-effect [:fx2]]
            [:expand-action [:ax2]]
            [:exec-effect [:fx3]]
            [:exec-effect [:fx4]]]))))

(deftest interpolation-order
  (testing "Interpolates actions in dispatched order"
    (is (= (->> [[:ax1 [:interpolation/order]]
                 [:ax2 [:interpolation/order]]]
                (h/test-dispatch-order nexus/dispatch))
           [[:expand-action [:ax1 [:interpolated/during 0]]]
            [:exec-effect [:fx1]]
            [:exec-effect [:fx2]]
            [:expand-action [:ax2 [:interpolated/during 3]]]
            [:exec-effect [:fx3]]
            [:exec-effect [:fx4]]]))))
