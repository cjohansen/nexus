(ns counter.core
  (:require [nexus.core :as nexus]
            [nexus.inspector :as inspector]
            [replicant.dom :as r]))

(defn block [ms]
  (let [until (+ (inspector/now-ms) ms)]
    (while (< (inspector/now-ms) until))))

(def nexus
  {:nexus/system->state deref
   :nexus/effects
   {:effects/save
    (fn [_ system path v]
      (swap! system assoc-in path v))

    :effects/fail
    (fn [_ _]
      (throw (ex-info "Oh noes!" {:boom? true})))}

   :nexus/actions
   {:actions/step
    (fn [state path]
      [[:effects/save path (+ (get-in state path) (or (state :step-size) 1))]])

    :actions/step-slowly
    (fn [state path]
      (block 500)
      [[:effects/save path (+ (get-in state path) (or (state :step-size) 1))]])

    :actions/reset
    (fn [_ path]
      [[:effects/save path 0]])

    :actions/set-step-size
    (fn [_ s]
      [[:effects/save [:step-size] s]])

    :actions/plan-to-fail
    (fn [_]
      [[:effects/fail]])}

   :nexus/placeholders
   {:event.target/value
    #(some-> % :replicant/dom-event .-target .-value)

    :fmt/number
    (fn [_ value]
      (or (some-> value not-empty parse-long) 0))}})

(defn render-ui [state]
  [:div
   [:h1.text-lg "Counter"]
   [:div.flex.gap-4.items-center
    {:style {:display "flex"
             :flex-direction "column"
             :gap "0.5rem"}}
    [:div "Number is " (:number state)]
    [:div
     [:button.btn
      {:on {:click [[:actions/step [:number]]]}}
      "Count"]]
    [:div
     [:button.btn
      {:on {:click [[:actions/step [:number]]
                    [:actions/step [:number]]]}}
      "Count twice"]]
    [:div
     [:button.btn
      {:on {:click [[:actions/step-slowly [:number]]]}}
      "Count slowly"]]
    [:div
     [:button.btn
      {:on {:click [[:actions/plan-to-fail]]}}
      "Fail"]]
    [:div.flex.flex-col
     [:label
      "Step size"
      [:input.input
       {:on {:blur [[:actions/set-step-size [:fmt/number [:event.target/value]]]]}
        :value (:step-size state)}]]]
    [:div
     [:button.btn
      {:on {:click [[:actions/reset [:number]]]}}
      "Reset"]]]])

(defn start [nexus el system]
  (r/set-dispatch! #(nexus/dispatch nexus system (select-keys %1 [:replicant/dom-event]) %2))
  (add-watch system ::render #(r/render el (render-ui %4)))
  (swap! system assoc ::started-at #?(:cljs (js/Date.) :clj (java.util.Date.))))
