(ns nexus.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]
            [nexus.test-helper :as h]))

(def test-nexus
  {:nexus/system->state deref

   :nexus/placeholders
   {:dispatch/number (fn [{:keys [value]}] value)}

   :nexus/effects
   {:effects/save
    (fn [_ system path v]
      (swap! system assoc-in path v))}

   :nexus/actions
   {:actions/plus
    (fn [state path n]
      [[:effects/save path (+ (get-in state path 0) n)]])

    :actions/inc
    (fn [_ path]
      [[:actions/plus path 1]])}})

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

(deftest dispatch-test
  (testing "Fails on unknown action/effect"
    (is (= (->> [[:effects/no-such-thing]]
                (nexus/dispatch test-nexus (atom {}) {:value 5})
                h/datafy-errors)
           {:errors
            [{:phase :execute-effect
              :effect-k :effects/no-such-thing
              :err {:message "No such effect"
                    :data {:available-effects [:effects/save]}}}]})))

  (testing "Executes effect"
    (is (= (let [system (atom {})]
             (->> [[:effects/save [:number] 1]]
                  (nexus/dispatch test-nexus system {:value 5}))
             @system)
           {:number 1})))

  (testing "Expands and executes action"
    (is (= (let [system (atom {:value 0})]
             (->> [[:actions/plus [:value] 2]]
                  (nexus/dispatch test-nexus system {:value 5}))
             @system)
           {:value 2})))

  (testing "Double expands and executes action"
    (is (= (let [system (atom {:value 0})]
             (->> [[:actions/inc [:value]]]
                  (nexus/dispatch test-nexus system {:value 5}))
             @system)
           {:value 1})))

  (testing "Expands, interpolates and executes action"
    (is (= (let [system (atom {:number 2})]
             (->> [[:actions/plus [:number] [:dispatch/number]]]
                  (nexus/dispatch test-nexus system {:value 5}))
             @system)
           {:number 7})))

  (testing "Passes state to action handler"
    (is (= (-> test-nexus
               (assoc :nexus/actions
                      {:actions/test
                       (fn [state arg]
                         [[:effects/save (:path (:config state)) arg]])})
               (nexus/dispatch (atom {:config {:path [:number]}}) {} [[:actions/test "it"]])
               :results
               first
               :res)
           {:config {:path [:number]}
            :number "it"})))

  (testing "Returns error when action handler does not return collection of actions"
    (is (= (-> test-nexus
               (assoc-in
                [:nexus/actions :actions/test]
                (fn [{:keys [config]} arg]
                  [:actions/store (:n config) arg]))
               (nexus/dispatch (atom {:config {:n 2}}) {} [[:actions/test "it"]])
               h/datafy-errors)
           {:errors
            [{:action [:actions/test "it"]
              :trace [[:actions/test "it"]]
              :phase :expand-action
              :err {:message ":actions/test should expand to a collection of actions"
                    :data {:res [:actions/store 2 "it"]
                           :action [:actions/test "it"]}}}]})))

  (testing "Expands action to empty list of effects"
    (is (empty? (-> test-nexus
                    (assoc-in [:nexus/actions :actions/test]
                              (fn [_ _] []))
                    (nexus/dispatch (atom {:config {:n 2}}) {} [[:actions/test "it"]])))))

  (testing "Interpolates placeholders in expanded actions"
    (is (= (-> test-nexus
               (assoc-in [:nexus/actions :actions/inc]
                         (fn [_ path]
                           [[:actions/plus path [:placeholders/one]]]))
               (assoc-in [:nexus/placeholders :placeholders/one] :one)
               (nexus/dispatch (atom {:number 0}) {:one 11} [[:actions/inc [:number]]])
               :results)
           [{:effect [:effects/save [:number] 11]
             :res {:number 11}}])))

  (testing "Returns errors from bad action handler"
    (is (= (-> test-nexus
               (assoc-in [:nexus/actions :actions/inc]
                         (fn [_ _]
                           (throw (ex-info "Boom!" {}))))
               (nexus/dispatch (atom {}) {} [[:actions/inc 2]])
               h/datafy-errors)
           {:errors [{:phase :expand-action
                      :action [:actions/inc 2]
                      :trace [[:actions/inc 2]]
                      :err {:message "Boom!"
                            :data {}}}]})))

  (testing "Calls before-action interceptor before action handler"
    (is (= (-> (h/with-interceptor test-nexus :before-action
                 #(assoc-in % [:state :number] 5))
               (nexus/dispatch (atom {}) {} [[:actions/inc [:number]]]))
           {:results [{:effect [:effects/save [:number] 6]
                       :res {:number 6}}]})))

  (testing "Calls after-action interceptor after action handler"
    (is (= (let [log (atom [])]
             (-> (h/with-interceptor test-nexus :after-action
                   (fn [context]
                     (swap! log conj {:in (:action context) :out (:actions context)})
                     context))
                 (nexus/dispatch (atom {:number 0}) {} [[:actions/plus [:number] 2]]))
             @log)
           [{:in [:actions/plus [:number] 2]
             :out [[:effects/save [:number] 2]]}])))

  (testing "Does not call interceptors for actions that have no handlers"
    (is (= (let [log (atom [])]
             (-> (h/with-interceptor test-nexus :before-action
                   (fn [ctx]
                     (swap! log conj ctx)))
                 (nexus/dispatch (atom {:state "Here"}) {} [[:actions/unknown 2]]))
             @log)
           [])))

  (testing "Returns error from before-action interceptor"
    (let [system (atom {:state "Here"})]
      (is (= (-> (h/with-interceptor test-nexus :before-action
                   (fn [ctx]
                     (throw (ex-info "Boom!" {:ctx ctx}))))
                 (nexus/dispatch system {} [[:actions/plus [:number] 2]])
                 h/datafy-errors)
             {:errors
              [{:phase :before-action
                :trace [[:actions/plus [:number] 2]]
                :action [:actions/plus [:number] 2]
                :err
                {:message "Boom!"
                 :data
                 {:ctx
                  {:nexus 'nexus
                   :system system
                   :actions [[:actions/plus [:number] 2]]
                   :action [:actions/plus [:number] 2]
                   :state {:state "Here"}
                   :dispatch-data {}
                   :queue [{:phase :expand-action
                            :before-action ::h/fn}]
                   :stack [{:before-action ::h/fn}]
                   :trace [[:actions/plus [:number] 2]]}}}}]}))))

  (testing "Returns error from after-action interceptor"
    (let [system (atom {:number 0})]
      (is (= (-> (h/with-interceptor test-nexus :after-action
                   (fn [ctx]
                     (throw (ex-info "Boom!" {:ctx ctx})))
                   :logger)
                 (nexus/dispatch system {} [[:actions/plus [:number] 2]])
                 h/datafy-errors)
             {:errors
              [{:phase :after-action
                :id :logger
                :trace nil
                :action [:actions/plus [:number] 2]
                :err
                {:message "Boom!"
                 :data
                 {:ctx
                  {:nexus 'nexus
                   :system system
                   :actions [[:effects/save [:number] 2]]
                   :action [:actions/plus [:number] 2]
                   :state {:number 2}
                   :dispatch-data {}
                   :dispatch ::h/fn
                   :queue nil
                   :trace nil
                   :stack nil
                   :results [{:effect [:effects/save [:number] 2]
                              :res {:number 2}}]}}}}]}))))

  (testing "Allows interceptor to abort action expansion flow"
    (is (= (-> (h/with-interceptor test-nexus :before-action
                 #(throw (ex-info "Boom!" {:ctx %}))
                 :logger)
               (h/with-interceptor :before-action
                   #(cond-> %
                      (:errors %) (dissoc :queue :stack))
                 :abort-early)
               (nexus/dispatch (atom {:state "Here"}) {} [[:actions/inc 2]])
               h/datafy-errors
               (update-in [:errors 0 :err] select-keys [:message]))
           ;; No results, because no effects!
           {:errors
            [{:id :logger
              :phase :before-action
              :action [:actions/inc 2]
              :trace [[:actions/inc 2]]
              :err {:message "Boom!"}}]})))

  (testing "Preserves order of residual actions during recursive expansion"
    ;; A repro / regression test for the bug addressed by
    ;; https://github.com/cjohansen/nexus/pull/11
    ;; When recursively expanding actions, action order was being reversed
    (is (= (-> {:nexus/system->state deref

                :nexus/actions
                {:actions/outer (fn [_] [[:actions/inner] [:effects/tail]])
                 :actions/inner (fn [_] [[:effects/a] [:effects/b] [:effects/c]])}

                :nexus/effects
                {:effects/a (fn [_ _])
                 :effects/b (fn [_ _])
                 :effects/c (fn [_ _])
                 :effects/tail (fn [_ _])}}
               (nexus/dispatch (atom {}) {} [[:actions/outer]])
               :results
               (->> (map :effect)))
           [[:effects/a] [:effects/b] [:effects/c] [:effects/tail]])))

  (testing "Returns results"
    (is (= (let [system (atom {:number 1})]
             (->> [[:actions/inc [:number]]]
                  (nexus/dispatch test-nexus system {:value 5})
                  :results))
           [{:effect [:effects/save [:number] 2]
             :res {:number 2}}])))

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
                  :nexus/effects
                  {:effects/fail
                   (fn [_ _]
                     (throw (ex-info "Boom!" {})))
                   :effects/dispatch-fail
                   (fn [{:keys [dispatch]} _]
                     (dispatch [[:effects/fail]]))}}
                 (h/with-interceptor :after-effect
                     (fn [ctx]
                       (swap! intercepted-errors into (:errors ctx))
                       ctx))
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
                 (h/with-interceptor :after-effect
                     (fn [ctx]
                       (swap! intercepted-errors into (:errors ctx))
                       ctx))
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
    (is (= (let [system (atom {:number 3})
                 log (atom [])]
             (-> test-nexus
                 (assoc :nexus/interceptors [(h/log-interceptor log 1)
                                             (h/log-interceptor log 2)
                                             (h/log-interceptor log 3)])
                 (nexus/dispatch system {:value 5} [[:actions/plus [:number] [:dispatch/number]]]))
             @log)
           [[:before-dispatch 1 [[:actions/plus [:number] [:dispatch/number]]]]
            [:before-dispatch 2 [[:actions/plus [:number] [:dispatch/number]]]]
            [:before-dispatch 3 [[:actions/plus [:number] [:dispatch/number]]]]
            [:before-action 1 :actions/plus]
            [:before-action 2 :actions/plus]
            [:before-action 3 :actions/plus]
            [:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:number 8}]
            [:after-effect 2 :effects/save {:number 8}]
            [:after-effect 1 :effects/save {:number 8}]
            [:after-action 3 :actions/plus [:effects/save]]
            [:after-action 2 :actions/plus [:effects/save]]
            [:after-action 1 :actions/plus [:effects/save]]
            [:after-dispatch 3 [{:effect [:effects/save [:number] 8]
                                 :res {:number 8}}]]
            [:after-dispatch 2 [{:effect [:effects/save [:number] 8]
                                 :res {:number 8}}]]
            [:after-dispatch 1 [{:effect [:effects/save [:number] 8]
                                 :res {:number 8}}]]])))

  (testing "Runs interceptors for all effects"
    (is (= (let [log (atom [])]
             (-> (assoc-in test-nexus [:nexus/actions :actions/duplex]
                           (fn [_]
                             [[:effects/save [:a] 1]
                              [:effects/save [:b] 2]]))
                 (assoc :nexus/interceptors [(h/log-interceptor log 1)])
                 (nexus/dispatch (atom {}) {:value 5} [[:actions/duplex]]))
             @log)
           [[:before-dispatch 1 [[:actions/duplex]]]
            [:before-action 1 :actions/duplex]
            [:before-effect 1 :effects/save]
            [:after-effect 1 :effects/save {:a 1}]
            [:before-effect 1 :effects/save]
            [:after-effect 1 :effects/save {:a 1 :b 2}]
            [:after-action 1 :actions/duplex [:effects/save :effects/save]]
            [:after-dispatch 1
             [{:effect [:effects/save [:a] 1]
               :res {:a 1}}
              {:effect [:effects/save [:b] 2]
               :res {:a 1, :b 2}}]]])))

  (testing "Dispatches actions recursively with new dispatch data"
    (is (= (let [system (atom {:step-size 3})
                 log (atom [])]
             (-> test-nexus
                 (assoc
                  :nexus/placeholders {:dd/k (fn [dispatch-data k]
                                               (if (contains? dispatch-data k)
                                                 (k dispatch-data)
                                                 [:dd/k k]))}
                  :nexus/interceptors [(h/log-interceptor log 1)]
                  :nexus/effects
                  {:effects/save
                   (fn [{:keys [dispatch]} system path v & [{:keys [on-success]}]]
                     (let [res (swap! system assoc-in path v)]
                       (when on-success
                         (dispatch on-success res))
                       res))})
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

  (testing "Interceptors can track state in nested dispatch"
    (is (= (let [log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/interceptors [{:before-dispatch
                                        (fn [ctx]
                                          (let [id (count @log)]
                                            (swap! log conj (cond-> {:id id}
                                                              (:id ctx) (assoc :parent (:id ctx))))
                                            (-> (assoc ctx :id id)
                                                (update :dispatch-trace conj id))))}]
                  :nexus/actions {:actions/prepare-it (fn [_] [[:effects/doit]])
                                  :actions/noop (fn [_] [])}
                  :nexus/effects
                  {:effects/doit
                   (fn [{:keys [dispatch]} _]
                     (dispatch [[:actions/noop]]))}}
                 (nexus/dispatch (atom {}) {} [[:actions/prepare-it]]))
             @log)
           [{:id 0}
            {:id 1
             :parent 0}])))

  (testing "Can hijack the dispatch function with an interceptor"
    (is (= (let [system (atom {})]
             (-> {:nexus/system->state deref
                  :nexus/effects
                  {:effects/create
                   (fn [{:keys [dispatch]} system path v]
                     (swap! system assoc-in path v)
                     (dispatch [[:effects/save (conj path :created?) true]]))

                   :effects/save
                   (fn [_ system path v]
                     (swap! system assoc-in path v))}}
                 (h/with-interceptor :before-effect
                     (fn [ctx]
                       (update ctx :dispatch (fn [dispatch]
                                               (fn [actions & _]
                                                 (dispatch (conj actions [:effects/save [:log] "Sneaky!"])))))))
                 (nexus/dispatch system {} [[:effects/create [:person] {:name "Chris"}]]))
             @system)
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
                     (swap! system assoc-in k v))}}
                 (h/with-interceptor :before-effect
                     (fn [ctx]
                       (swap! log conj (second (:effect ctx)))
                       ctx))
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
             (-> test-nexus
                 (assoc
                  :nexus/system->state deref
                  :nexus/interceptors [{:before-dispatch #(assoc % :dispatch-id "dispatch-1")
                                        :before-action #(assoc % :action-id "action-1")
                                        :before-effect #(assoc % :effect-id "effect-1")
                                        :after-dispatch #(do (swap! log conj (select-keys % [:dispatch-id :action-id :effect-id]))
                                                             %)}])
                 (nexus/dispatch system {:value 5} [[:actions/inc [:number]]]))
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
                  {:actions/noop (constantly nil)}}
                 (h/with-interceptor :before-action
                     (fn [ctx]
                       (reset! state (:state ctx))
                       ctx))
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
