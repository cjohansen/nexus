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

(defn ^{:indent 2} with-interceptor [nexus phase f & [id]]
  (update nexus :nexus/interceptors (fnil conj []) (cond-> {phase f}
                                                     id (assoc :id id))))

(def nexus-with-inc
  {:nexus/actions
   {:actions/inc
    (fn [state n]
      [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}})

(defn log-interceptor [log n]
  {:id n
   :before-action (fn [in]
                    (swap! log conj [:before-action n (get-in in [:action 0])])
                    in)
   :after-action (fn [in]
                   (swap! log conj (cond-> [:after-action n (get-in in [:action 0])]
                                     (seq (:actions in)) (conj (mapv first (:actions in)))))
                   in)
   :before-effect (fn [in]
                    (swap! log conj [:before-effect n
                                     (first (or (:effect in) (first (:effects in))))])
                    in)
   :after-effect (fn [in]
                   (swap! log conj [:after-effect n
                                    (first (or (:effect in) (first (:effects in))))
                                    (:res in)])
                   in)
   :before-dispatch (fn [in]
                      (swap! log conj [:before-dispatch n (:actions in)])
                      in)
   :after-dispatch (fn [in]
                     (swap! log conj [:after-dispatch n (:results in)])
                     in)})

(deftest expand-actions-test
  (testing "Noops without any expansions"
    (is (= (->> [[:actions/test :it]]
                (nexus/expand-actions {} {})
                :effects)
           [[:actions/test :it]])))

  (testing "Expands action"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [_ arg]
                   [[:actions/store (str/upper-case arg)]])}}
               (nexus/expand-actions {} [[:actions/test "it"]])
               :effects)
           [[:actions/store "IT"]])))

  (testing "Passes state to action handler"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [state arg]
                   [[:actions/store (:n (:config state)) arg]])}}
               (nexus/expand-actions {:config {:n 2}} [[:actions/test "it"]])
               :effects)
           [[:actions/store 2 "it"]])))

  (testing "Returns error when action handler does not return collection of actions"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [{:keys [config]} arg]
                   [:actions/store (:n config) arg])}}
               (nexus/expand-actions {:config {:n 2}} [[:actions/test "it"]])
               h/datafy-errors)
           {:errors
            [{:action [:actions/test "it"]
              :phase :expand-action
              :err {:message ":actions/test should expand to a collection of actions"
                    :data {:res [:actions/store 2 "it"]}}}]})))

  (testing "Expands action to empty list of effects"
    (is (empty? (-> {:nexus/actions
                     {:actions/test
                      (fn [_ _] [])}}
                    (nexus/expand-actions {:config {:n 2}} [[:actions/test "it"]])
                    :effects))))

  (testing "Expands actions recursively"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ n]
                   [[:actions/plus n 1]])
                 :actions/plus
                 (fn [_ a b]
                   [[:actions/store "n" (+ a b)]])}}
               (nexus/expand-actions {} [[:actions/inc 2]])
               :effects)
           [[:actions/store "n" 3]])))

  (testing "Returns errors from bad action handler"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))}}
               (nexus/expand-actions {} [[:actions/inc 2]])
               h/datafy-errors)
           {:errors [{:phase :expand-action
                      :action [:actions/inc 2]
                      :err {:message "Boom!"
                            :data {}}}]})))

  (testing "Calls before-interceptor before action handler"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 #(assoc-in % [:state :base-n] 2))
               (nexus/expand-actions {} [[:actions/inc 9]]))
           {:effects [[:effects/save [:number] 12]]})))

  (testing "Calls after-interceptor after action handler"
    (is (= (let [log (atom [])]
             (-> (with-interceptor nexus-with-inc :after-action
                   (fn [context]
                     (swap! log conj {:in (:action context) :out (:actions context)})
                     context))
                 (nexus/expand-actions {} [[:actions/inc 9]]))
             @log)
           [{:in [:actions/inc 9]
             :out [[:effects/save [:number] 10]]}])))

  (testing "Does not call interceptors for actions that have no handlers"
    (is (= (let [log (atom [])]
             (-> {:nexus/interceptors
                  [{:before-action
                    (fn [ctx]
                      (swap! log conj ctx))}]}
                 (nexus/expand-actions {:state "Here"} [[:actions/inc 2]]))
             @log)
           [])))

  (testing "Returns error from before-action interceptor"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 (fn [ctx]
                   (throw (ex-info "Boom!" {:ctx ctx}))))
               (nexus/expand-actions {:state "Here"} [[:actions/inc 2]])
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
              :action [:actions/inc 2]}]})))

  (testing "Returns error from after-action interceptor"
    (is (= (-> (with-interceptor nexus-with-inc :after-action
                 (fn [ctx]
                   (throw (ex-info "Boom!" {:ctx ctx})))
                 :logger)
               (nexus/expand-actions {:state "Here"} [[:actions/inc 2]])
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
              :action [:actions/inc 2]}]})))

  (testing "Allows interceptor to abort action expansion flow"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 #(throw (ex-info "Boom!" {:ctx %}))
                 :logger)
               (with-interceptor :before-action
                   #(cond-> %
                      (:errors %) (dissoc :queue :stack))
                 :abort-early)
               (nexus/expand-actions {:state "Here"} [[:actions/inc 2]])
               h/datafy-errors
               (update-in [:errors 0 :err] select-keys [:message]))
           ;; No effects!
           {:errors
            [{:id :logger
              :phase :before-action
              :action [:actions/inc 2]
              :err {:message "Boom!"}}]})))

  (testing "Calls interceptors in order"
    (is (= (let [log (atom [])]
             (-> {:nexus/actions
                  {:actions/inc
                   (fn [state n]
                     (swap! log conj [:action])
                     [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}
                  :nexus/interceptors [(log-interceptor log 1)
                                       (log-interceptor log 2)
                                       (log-interceptor log 3)]}
                 (nexus/expand-actions {} [[:actions/inc 9]]))
             @log)
           [[:before-action 1 :actions/inc]
            [:before-action 2 :actions/inc]
            [:before-action 3 :actions/inc]
            [:action]
            [:after-action 3 :actions/inc [:effects/save]]
            [:after-action 2 :actions/inc [:effects/save]]
            [:after-action 1 :actions/inc [:effects/save]]]))))

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
                  :nexus/action)))))

(def nexus-with-save
  {:nexus/effects
   {:effects/save
    (fn [_ store path v]
      (swap! store assoc-in path v))}})

(def nexus-with-batched-save
  {:nexus/effects
   {:effects/save
    ^:nexus/batch?
    (fn [_ store path-vs]
      (swap! store
             (fn [state]
               (reduce (fn [s [p v]] (assoc-in s p v)) state path-vs))))}})

(deftest execute-test
  (testing "Fails when there is no implementation"
    (is (= (-> (nexus/execute {} {:system (atom {})} [[:effects/save [:number] 3]])
               h/datafy-errors)
           {:errors [{:phase :execute-effect
                      :effect-k :effects/save
                      :err {:message "No such effect"
                            :data {:available-effects nil}}}]})))

  (testing "Returns result of executing effect"
    (is (= (-> {:nexus/effects
                {:effects/save
                 (fn [_ store path v]
                   (swap! store assoc-in path v))}}
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [[:effects/save [:number] 3]]))
           {:results
            [{:effect [:effects/save [:number] 3]
              :res {:existing "Data"
                    :number 3}}]})))

  (testing "Processes effects one by one"
    (is (= (-> nexus-with-save
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [[:effects/save [:number] 3]
                 [:effects/save [:name] "Nexus"]]))
           {:results
            [{:effect [:effects/save [:number] 3]
              :res {:existing "Data"
                    :number 3}}
             {:effect [:effects/save [:name] "Nexus"]
              :res {:existing "Data"
                    :number 3
                    :name "Nexus"}}]})))

  (testing "Optionally batch processes effect"
    (is (= (-> nexus-with-batched-save
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [[:effects/save [:number] 3]
                 [:effects/save [:name] "Nexus"]]))
           {:results
            [{:effects [[:effects/save [:number] 3]
                        [:effects/save [:name] "Nexus"]]
              :res {:existing "Data"
                    :number 3
                    :name "Nexus"}}]})))

  (testing "Calls interceptors in order"
    (is (= (let [log (atom [])]
             (-> nexus-with-save
                 (assoc :nexus/interceptors
                        [(log-interceptor log 1)
                         (log-interceptor log 2)
                         (log-interceptor log 3)])
                 (nexus/execute
                  {:system (atom {:existing "Data"})}
                  [[:effects/save [:number] 9]]))
             @log)
           [[:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:existing "Data", :number 9}]
            [:after-effect 2 :effects/save {:existing "Data", :number 9}]
            [:after-effect 1 :effects/save {:existing "Data", :number 9}]])))

  (testing "Calls interceptors for batch in order"
    (is (= (let [log (atom [])]
             (-> nexus-with-batched-save
                 (assoc :nexus/interceptors
                        [(log-interceptor log 1)
                         (log-interceptor log 2)
                         (log-interceptor log 3)])
                 (nexus/execute
                  {:system (atom {:existing "Data"})}
                  [[:effects/save [:number] 9]]))
             @log)
           [[:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:existing "Data", :number 9}]
            [:after-effect 2 :effects/save {:existing "Data", :number 9}]
            [:after-effect 1 :effects/save {:existing "Data", :number 9}]]))))

(deftest dispatch-test
  (testing "Expands, interpolates and executes action"
    (is (= (let [store (atom {:step-size 3})]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dispatch/number (fn [{:keys [value]}] value)}}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch store {:value 5} [[:actions/inc [:dispatch/number]]]))
             @store)
           {:step-size 3
            :number 6})))

  (testing "Returns results"
    (is (= (let [store (atom {:step-size 3})]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dispatch/number (fn [{:keys [value]}] value)}}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch store {:value 5} [[:actions/inc [:dispatch/number]]])))
           {:results [{:effect [:effects/save [:number] 6]
                       :res {:step-size 3
                             :number 6}}]})))

  (testing "Runs interceptors in order"
    (is (= (let [store (atom {:step-size 3})
                 log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dispatch/number (fn [{:keys [value]}] value)}
                  :nexus/interceptors [(log-interceptor log 1)
                                       (log-interceptor log 2)
                                       (log-interceptor log 3)]}
                 (merge nexus-with-inc)
                 (merge nexus-with-save)
                 (nexus/dispatch store {:value 5} [[:actions/inc [:dispatch/number]]]))
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
    (is (= (let [store (atom {:step-size 3})
                 log (atom [])]
             (-> {:nexus/system->state deref
                  :nexus/placeholders {:dd/k (fn [dispatch-data k]
                                               (if (contains? dispatch-data k)
                                                 (k dispatch-data)
                                                 [:dd/k k]))}
                  :nexus/interceptors [(log-interceptor log 1)]
                  :nexus/effects {:effects/save
                                  (fn [{:keys [dispatch]} store path v & [{:keys [on-success]}]]
                                    (let [res (swap! store assoc-in path v)]
                                      (when on-success
                                        (dispatch on-success res))
                                      res))}}
                 (merge nexus-with-inc)
                 (nexus/dispatch store {:value 5}
                     [[:effects/save [:number] [:dd/k :value]
                       {:on-success [[:effects/save [:second-number] [:dd/k :number]]]}]]))
             [@store @log])
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
                                  :res {:number 5, :step-size 3}}]]]]))))
