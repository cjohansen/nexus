(ns nexus.serial-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]
            [nexus.serial :as serial]))

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
