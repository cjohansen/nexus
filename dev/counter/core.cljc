(ns counter.core
  (:require [nexus.core :as nexus]
            [replicant.dom :as r]))

(def nexus
  {:system->state deref
   :effects
   {:actions/save
    (fn [_ store path v]
      (swap! store assoc-in path v))}

   :actions
   {:actions/step
    (fn [state path]
      [[:actions/save path (+ (get-in state path) (or (state :step-size) 1))]])

    :actions/reset
    (fn [_ path]
      [[:actions/save path 0]])

    :actions/set-step-size
    (fn [_ s]
      [[:actions/save [:step-size] s]])}

   :placeholders
   {:event.target/value
    #(some-> % :replicant/dom-event .-target .-value)

    :fmt/number
    (fn [_ value]
      (or (some-> value not-empty parse-long) 0))}})

(defn render-ui [state]
  [:div
   [:h1.text-lg "Counter"]
   [:div.flex.gap-4.items-center
    [:div "Number is " (:number state)]
    [:button.btn
     {:on {:click [[:actions/step [:number]]]}}
     "Count!"]
    [:div.flex.flex-col
     [:label
      "Step size"
      [:input.input
       {:on {:blur [[:actions/set-step-size [:fmt/number [:event.target/value]]]]}
        :value (:step-size state)}]]]
    [:button.btn
     {:on {:click [[:actions/reset [:number]]]}}
     "Reset"]]])

(defn start [nexus el store]
  (r/set-dispatch! #(nexus/dispatch nexus store (select-keys %1 [:replicant/dom-event]) %2))
  (add-watch store ::render #(r/render el (render-ui %4)))
  (swap! store assoc ::started-at #?(:cljs (js/Date.) :clj (java.util.Date.))))
