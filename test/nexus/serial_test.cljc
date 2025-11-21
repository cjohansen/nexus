(ns nexus.serial-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]
            [nexus.serial :as serial]
            [nexus.test-helper :as h]))

(def !store (atom {}))

(def nexus-map
  {:nexus/system->state (fn [{:keys [!store]}] @!store)

   :nexus/interceptors [h/dispatch-order-interceptor]

   :nexus/placeholders
   {:interpolate/me
    (fn [_state]
      :interpolated)}

   :nexus/effects
   {:fx1 (fn [_ctx _sys] nil)
    :fx2 (fn [_ctx _sys] nil)
    :fx3 (fn [_ctx _sys] nil)
    :fx4 (fn [_ctx _sys] nil)}

   :nexus/actions
   {:ax1 (fn [_state] [[:fx1]
                        [:fx2]])
    :ax2 (fn [_state] [[:fx3]
                        [:fx4]])}})

(defn dispatch [actions]
  (serial/dispatch nexus-map {:!store !store} {} actions))

(defn dispatch-default [actions]
  (nexus/dispatch nexus-map {:!store !store} {} actions))

(deftest serial-dispatch-order
  (testing "Should execute effects prior to action expansion"
    (let [_result (dispatch [[:fx1] [:fx2] [:ax1]])]
      (is (= [[:dispatch [[:fx1] [:fx2] [:ax1]]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:expand-action [:ax1]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]]
             @h/!dispatch-history))))
  (testing "Should expand actions lazily"
    (let [_result (dispatch [[:ax1] [:ax2]])]
      (is (= [[:dispatch [[:ax1] [:ax2]]]
              [:expand-action [:ax1]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:expand-action [:ax2]]
              [:exec-effect [:fx3]]
              [:exec-effect [:fx4]]]
             @h/!dispatch-history)))))

(deftest default-dispatch-order
  (testing "Should expand all actions prior to effect execution"
    (let [_result (dispatch-default [[:fx1] [:fx2] [:ax1]])]
      (is (= [[:dispatch [[:fx1] [:fx2] [:ax1]]]
              [:expand-action [:ax1]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]]
             @h/!dispatch-history))))
  (testing "Should expand actions eagerly"
    (let [_result (dispatch-default [[:ax1] [:ax2]])]
      (is (= [[:dispatch [[:ax1] [:ax2]]]
              [:expand-action [:ax1]]
              [:expand-action [:ax2]]
              [:exec-effect [:fx1]]
              [:exec-effect [:fx2]]
              [:exec-effect [:fx3]]
              [:exec-effect [:fx4]]]
             @h/!dispatch-history)))))

;;TODO: Add tests verifying interpolation happens eagerly for default dispatch and just-in-time for serial dispatch
