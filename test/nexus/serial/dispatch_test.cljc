(ns nexus.serial.dispatch-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]
            [nexus.serial.dispatch :as serial]
            [nexus.test-helper :as h]))

(def nexus-with-inc
  {:nexus/actions
   {:actions/inc
    (fn [state n]
      [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}})

(deftest dispatch-order
  (testing "Executes effects prior to action expansion"
    (is (= (h/test-dispatch-order serial/dispatch [[:fx1] [:fx2] [:ax1]])
           [[:exec-effect [:fx1]]
            [:exec-effect [:fx2]]
            [:expand-action [:ax1]]
            [:exec-effect [:fx1]]
            [:exec-effect [:fx2]]])))

  (testing "Expands actions lazily"
    (is (= (h/test-dispatch-order serial/dispatch [[:ax1] [:ax2]])
           [[:expand-action [:ax1]]
            [:exec-effect [:fx1]]
            [:exec-effect [:fx2]]
            [:expand-action [:ax2]]
            [:exec-effect [:fx3]]
            [:exec-effect [:fx4]]]))))

(deftest interpolation-order
  (testing "Interpolates actions in dispatched order"
    (is (= (h/test-dispatch-order serial/dispatch [[:ax1 [:interpolation/order]]
                                                   [:ax2 [:interpolation/order]]])
           [[:expand-action [:ax1 [:interpolated/during 0]]]
            [:exec-effect [:fx1]]
            [:exec-effect [:fx2]]
            [:expand-action [:ax2 [:interpolated/during 3]]]
            [:exec-effect [:fx3]]
            [:exec-effect [:fx4]]]))))

(deftest parity-test
  (testing "Has the same return value as the batch strategy"
    (let [nexus {:nexus/system->state deref
                 :nexus/effects
                 {:effects/save
                  (fn [_ store path v]
                    (swap! store assoc-in path v))}}
          actions [[:effects/save [:number] 3]]]
      (is (= (serial/dispatch nexus (atom {}) {} actions)
             (nexus/dispatch nexus (atom {}) {} actions))))))

(deftest expand-actions-test
  (testing "Noops without any expansions"
    (is (= (->> {:actions [[:actions/test :it]]}
                (serial/expand-lazily {})
                :effects)
           [[:actions/test :it]])))

  (testing "Expands action"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [_ arg]
                   [[:actions/store (str/upper-case arg)]])}}
               (serial/expand-lazily {:actions [[:actions/test "it"]]})
               :effects)
           [[:actions/store "IT"]])))

  (testing "Passes state to action handler"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [state arg]
                   [[:actions/store (:n (:config state)) arg]])}}
               (serial/expand-lazily {:state {:config {:n 2}} :actions [[:actions/test "it"]]})
               :effects)
           [[:actions/store 2 "it"]])))

  (testing "Returns error when action handler does not return collection of actions"
    (is (= (-> {:nexus/actions
                {:actions/test
                 (fn [{:keys [config]} arg]
                   [:actions/store (:n config) arg])}}
               (serial/expand-lazily {:state {:config {:n 2}} :actions [[:actions/test "it"]]})
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
                    (serial/expand-lazily {:state {:config {:n 2}} :actions [[:actions/test "it"]]})
                    :effects))))

  (testing "Expands actions recursively"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ n]
                   [[:actions/plus n 1]])
                 :actions/plus
                 (fn [_ a b]
                   [[:actions/store "n" (+ a b)]])}}
               (serial/expand-lazily {:actions [[:actions/inc 2]]})
               :effects)
           [[:actions/store "n" 3]])))

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
               (serial/expand-lazily {:actions [[:actions/inc 2]] :dispatch-data {:one 1}})
               :effects)
           [[:actions/store "n" 3]])))

  (testing "Returns errors from bad action handler"
    (is (= (-> {:nexus/actions
                {:actions/inc
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))}}
               (serial/expand-lazily {:actions [[:actions/inc 2]]})
               h/datafy-errors)
           {:errors
            [{:phase :expand-action
              :action [:actions/inc 2]
              :err {:message "Boom!"
                    :data {}}}]})))

  (testing "Calls before-interceptor before action handler"
    (is (= (-> (h/with-interceptor nexus-with-inc :before-action
                 #(assoc-in % [:state :base-n] 2))
               (nexus/expand-actions {} [[:actions/inc 9]])
               :effects)
           [[:effects/save [:number] 12]])))

  (testing "Calls after-interceptor after action handler"
    (is (= (let [log (atom [])]
             (-> (h/with-interceptor nexus-with-inc :after-action
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
    (is (= (-> (h/with-interceptor nexus-with-inc :before-action
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
    (is (= (-> (h/with-interceptor nexus-with-inc :after-action
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
    (is (= (-> (h/with-interceptor nexus-with-inc :before-action
                 #(throw (ex-info "Boom!" {:ctx %}))
                 :logger)
               (h/with-interceptor :before-action
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
                  :nexus/interceptors [(h/log-interceptor log 1)
                                       (h/log-interceptor log 2)
                                       (h/log-interceptor log 3)]}
                 (nexus/expand-actions {} [[:actions/inc 9]]))
             @log)
           [[:before-action 1 :actions/inc]
            [:before-action 2 :actions/inc]
            [:before-action 3 :actions/inc]
            [:action]
            [:after-action 3 :actions/inc [:effects/save]]
            [:after-action 2 :actions/inc [:effects/save]]
            [:after-action 1 :actions/inc [:effects/save]]]))))
