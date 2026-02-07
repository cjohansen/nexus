(ns nexus.serial-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]
            [nexus.serial :as serial]
            [nexus.test-helper :as h]))

;; Interceptor to verify action expansion and effect execution order
;;... expansion and execution order for last dispatch
(defonce !last-dispatch-order (atom []))
;;... cumulative history over multiple dispatches
(defonce !dispatch-history (atom []))

(def dispatch-history-interceptor
  {:id ::dispatch-order
   :before-dispatch (fn [{:keys [actions] :as ctx}]
                      (swap! !dispatch-history conj [:dispatch actions])
                      (reset! !last-dispatch-order [])
                      ctx)
   :after-dispatch (fn [ctx]
                     (swap! !dispatch-history into @!last-dispatch-order)
                     ctx)
   :before-action (fn [{:keys [action] :as ctx}]
                    (swap! !last-dispatch-order conj [:expand-action action])
                    ctx)
   :before-effect (fn [{:keys [effect] :as ctx}]
                    (swap! !last-dispatch-order conj [:exec-effect effect])
                    ctx)})


(def !store (atom {:executions 0}))
(def nexus-map
  {:nexus/system->state (fn [{:keys [!store]}] @!store)

   :nexus/interceptors [dispatch-history-interceptor]

   :nexus/placeholders
   {:interpolate/me
    (fn [_] :interpolated)
    :interpolation/order
    (fn [_] [:interpolated/during (count @!last-dispatch-order) #_(count @!executions)])}

   :nexus/effects
   {:fx1 (fn [_ctx _sys] nil)
    :fx2 (fn [_ctx _sys] nil)
    :fx3 (fn [_ctx _sys] nil)
    :fx4 (fn [_ctx _sys] nil)}

   :nexus/actions
   {:ax1 (fn [_state & _opts] [[:fx1]
                              [:fx2]])
    :ax2 (fn [_state & _opts] [[:fx3]
                              [:fx4]])}})

(defn dispatch [actions]
  (serial/dispatch nexus-map {:!store !store} {} actions))

(defn dispatch-default [actions]
  (nexus/dispatch nexus-map {:!store !store} {} actions))

(deftest serial-dispatch-order
  (testing "Should execute effects prior to action expansion"
    (let [_result (dispatch [[:fx1] [:fx2] [:ax1]])]
      (is (= @!last-dispatch-order
             [[:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:expand-action [:ax1]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]]))))
  (testing "Should expand actions lazily"
    (let [_result (dispatch [[:ax1] [:ax2]])]
      (is (= @!last-dispatch-order
             [[:expand-action [:ax1]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:expand-action [:ax2]]
              [:exec-effect [:fx3]]
              [:exec-effect [:fx4]]])))))

(deftest default-dispatch-order
  (testing "Should expand all actions prior to effect execution"
    (let [_result (dispatch-default [[:fx1] [:fx2] [:ax1]])]
      (is (= @!last-dispatch-order
             [[:expand-action [:ax1]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]]))))
  (testing "Should expand actions eagerly"
    (let [_result (dispatch-default [[:ax1] [:ax2]])]
      (is (= @!last-dispatch-order
             [[:expand-action [:ax1]]
              [:expand-action [:ax2]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:exec-effect [:fx3]]
              [:exec-effect [:fx4]]])))))

;; These next two tests are probably redundant
(deftest serial-interpolation
  (testing "Should interpolate actions in dispatched order"
    (let [_result (dispatch [[:ax1 [:interpolation/order]]
                             [:ax2 [:interpolation/order]]])]
      (is (= @!last-dispatch-order
             [[:expand-action [:ax1 [:interpolated/during 0]]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:expand-action [:ax2 [:interpolated/during 3]]]
              [:exec-effect [:fx3]]
              [:exec-effect [:fx4]]])))))

(deftest default-interpolation
  (testing "Should interpolate actions in dispatched order"
    (let [_result (dispatch-default [[:ax1 [:interpolation/order]]
                                     [:ax2 [:interpolation/order]]])]
      (is (= @!last-dispatch-order
             [[:expand-action [:ax1 [:interpolated/during 0]]]
              [:expand-action [:ax2 [:interpolated/during 1]]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:exec-effect [:fx3]]
              [:exec-effect [:fx4]]])))))

;;Test cases from core here....

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
           {:errors [{:phase :expand-action
                      :action [:actions/inc 2]
                      :err {:message "Boom!"
                            :data {}}}]})))

  #_(testing "Calls before-interceptor before action handler"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 #(assoc-in % [:state :base-n] 2))
               (nexus/expand-actions {} [[:actions/inc 9]])
               :effects)
           [[:effects/save [:number] 12]])))

  #_(testing "Calls after-interceptor after action handler"
    (is (= (let [log (atom [])]
             (-> (with-interceptor nexus-with-inc :after-action
                   (fn [context]
                     (swap! log conj {:in (:action context) :out (:actions context)})
                     context))
                 (nexus/expand-actions {} [[:actions/inc 9]]))
             @log)
           [{:in [:actions/inc 9]
             :out [[:effects/save [:number] 10]]}])))

  #_(testing "Does not call interceptors for actions that have no handlers"
    (is (= (let [log (atom [])]
             (-> {:nexus/interceptors
                  [{:before-action
                    (fn [ctx]
                      (swap! log conj ctx))}]}
                 (nexus/expand-actions {:state "Here"} [[:actions/inc 2]]))
             @log)
           [])))

  #_(testing "Returns error from before-action interceptor"
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

  #_(testing "Returns error from after-action interceptor"
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

  #_(testing "Allows interceptor to abort action expansion flow"
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

  #_(testing "Calls interceptors in order"
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
