(ns nexus.portfolio.action-log-scenes
  (:require [dataspex.ui :as ui]
            [nexus.hiccup-helper :as hh]
            [nexus.inspector :as inspector]
            [portfolio.replicant :refer [defscene]]))

(def add-task-tx
  {:action [:db/save {:task/id "task-id"
                      :task/title "Do chores"}]
   :expansions [{:effect [:db/transact
                          [{:task/id "task-id"
                            :task/title "Do chores"}]]
                 :state {:tasks []
                         :transient {:syncing {"task-id" true}}}
                 :result [1 :form/id "form" 536870913 true]
                 :effect-elapsed {:ms 1.0 :slow? false}}]})

(def add-task-action
  {:action [:task/add "task-id" {:task/title [:input/value]}]
   :interpolated [:task/add "task-id" {:task/title "Do chores"}]
   :interpolations {[:input/value] "Do chores"}
   :interpolation-elapsed (inspector/->timing 12)
   :expansion-elapsed (inspector/->timing 23)
   :expansions [{:action [:dev/log :create-task "task-id" "Do chores"]
                 :expansions [{:effect [:dev/log :create-task "task-id" "Do chores"]
                               :result nil
                               :state {:tasks []
                                       :transient {:syncing {"task-id" true}}}
                               :effect-elapsed {:ms 1.0 :slow? false}}]
                 :state {:tasks []
                         :transient {:syncing {"task-id" true}}}
                 :expansion-elapsed {:ms 0.1, :slow? false}}
                add-task-tx]
   :state {:tasks []
           :transient {:syncing {"task-id" true}}}})

(def log
  {:chronology '(#uuid "2b105ea8-99cc-4d7b-aabf-fc2568c56d0a"
                 #uuid "29e14b68-a5a8-4eac-8ed8-ab58df40480f"
                 #uuid "81425764-6219-43f1-aa61-07110ea16fca"
                 #uuid "98622ecc-26a8-440d-ab7c-26d347e8ed30"
                 #uuid "110b460f-57b2-4e15-be8c-359149d3db62"
                 #uuid "71ec2f56-7f08-417c-89b5-f7cf0710b1e5")

   :entries
   {#uuid "71ec2f56-7f08-417c-89b5-f7cf0710b1e5"
    {:id #uuid "71ec2f56-7f08-417c-89b5-f7cf0710b1e5"
     :dispatched-at #inst "2025-06-11T08:00:45"
     :dispatch-elapsed (inspector/->timing 42)
     :actions [{:action
                [:actions/execute-command
                 {:command/data {:person/tiltalenavn "Christian"}
                  :command/kind :commands/sett-mitt-tiltalenavn}]
                :expansions
                [{:effect [:actions/execute-command
                           {:command/data {:person/tiltalenavn "Christian"}
                            :command/kind :commands/sett-mitt-tiltalenavn}]
                  :effect-elapsed {:ms 1.0 :slow? false}}]}]}

    #uuid "110b460f-57b2-4e15-be8c-359149d3db62"
    {:id #uuid "110b460f-57b2-4e15-be8c-359149d3db62"
     :dispatched-at #inst "2025-06-11T08:01:12"
     :dispatch-elapsed (inspector/->timing 37)
     :actions [{:action
                [:form/submit
                 [:checkout-form "order-123"]
                 {:customer
                  {:customer/id "cust-1842"
                   :name "Jane Doe"
                   :email "jane@example.com"
                   :phone "+1 555 123 9911"
                   :addresses
                   [{:address/id "addr-1"
                     :type :shipping
                     :street "123 Main Street"
                     :city "Springfield"
                     :postal-code "90210"
                     :country "US"}
                    {:address/id "addr-2"
                     :type :billing
                     :street "456 Oak Avenue"
                     :city "Springfield"
                     :postal-code "90210"
                     :country "US"}]}

                  :cart
                  {:cart/id "cart-991"
                   :currency "USD"
                   :items
                   [{:product/id "sku-1"
                     :product/name "Mechanical Keyboard"
                     :quantity 1
                     :unit-price 14900
                     :total 14900}
                    {:product/id "sku-2"
                     :product/name "USB-C Cable"
                     :quantity 2
                     :unit-price 1900
                     :total 3800}
                    {:product/id "sku-3"
                     :product/name "Laptop Stand"
                     :quantity 1
                     :unit-price 6900
                     :total 6900}]
                   :discounts
                   [{:discount/code "SUMMER25"
                     :discount/amount 2500}]
                   :totals
                   {:subtotal 25600
                    :discounts -2500
                    :tax 4618
                    :shipping 1200
                    :grand-total 28918}}

                  :delivery
                  {:method :express
                   :estimated-delivery "2026-05-27"
                   :instructions "Leave package by garage door"}

                  :validation/errors
                  {:customer/email nil
                   :payment/card-number nil
                   :delivery/instructions
                   ["Instructions must be shorter than 120 characters"]}

                  :analytics/context
                  {:session/id "sess-1192"
                   :experiment-variants {:new-checkout-flow :variant-b
                                         :upsell-banner :control}
                   :utm {:source "newsletter"
                         :campaign "summer-sale"}}

                  :ui/state
                  {:submitting? true
                   :submitted-at "2026-05-24T10:12:18Z"
                   :active-step :review
                   :expanded-sections #{:shipping :payment :summary}}}]}]}

    #uuid "98622ecc-26a8-440d-ab7c-26d347e8ed30"
    {:id #uuid "98622ecc-26a8-440d-ab7c-26d347e8ed30"
     :dispatched-at #inst "2025-06-11T08:06:58"
     :dispatch-elapsed (inspector/->timing 31)
     :actions [add-task-action]
     :effects (:expansions add-task-tx)
     :errors [{}]}

    #uuid "81425764-6219-43f1-aa61-07110ea16fca"
    {:id #uuid "81425764-6219-43f1-aa61-07110ea16fca"
     :dispatched-at #inst "2025-06-11T08:07:23"
     :dispatch-elapsed (inspector/->timing 12)
     :dispatched-by {}
     :actions [add-task-action]
     :effects (:expansions add-task-tx)}

    #uuid "29e14b68-a5a8-4eac-8ed8-ab58df40480f"
    {:id #uuid "29e14b68-a5a8-4eac-8ed8-ab58df40480f"
     :dispatched-at #inst "2025-06-11T08:09:12"
     :dispatch-elapsed (inspector/->timing 233 {:slow? true})
     :dispatch-data {:number 42}
     :dispatches [#uuid "2b105ea8-99cc-4d7b-aabf-fc2568c56d0a"]
     :effects (concat
               [{:effect [:state/assoc-in [:transient :syncing "task-id"] true]
                 :effect-elapsed {:ms 1.0 :slow? false}
                 :state {:tasks []}
                 :result {:tasks []
                          :transient {:syncing {"task-id" true}}}
                 :dispatches [{:id #uuid "2b105ea8-99cc-4d7b-aabf-fc2568c56d0a"
                               :dispatched-at #inst "2025-06-11T08:09:13"
                               :actions [[:transient/assoc-in [:syncing "task-id"] false]]}]}
                {:effect [:dev/log :create-task "task-id" "Do chores"]
                 :effect-elapsed {:ms 1.0 :slow? false}
                 :state {:tasks []
                         :transient {:syncing {"task-id" true}}}
                 :result nil}]
               (:expansions add-task-tx))
     :actions
     [{:action [:transient/assoc-in [:syncing "task-id"] true]
       :expansions [{:effect [:state/assoc-in [:transient :syncing "task-id"] true]
                     :state {:tasks []}
                     :effect-elapsed {:ms 1.0 :slow? false}
                     :result {:tasks []
                              :transient {:syncing {"task-id" true}}}}]
       :expansion-elapsed (inspector/->timing 16)
       :state {:tasks []}}
      add-task-action]

     :dom-event #?(:cljs (let [event (js/MouseEvent. "click" #js {:bubbles true
                                                                  :cancelable true
                                                                  :view js/window})]
                           (js/document.body.dispatchEvent event)
                           event)
                   :clj nil)}

    #uuid "2b105ea8-99cc-4d7b-aabf-fc2568c56d0a"
    {:id #uuid "2b105ea8-99cc-4d7b-aabf-fc2568c56d0a"
     :dispatched-by {:id #uuid "29e14b68-a5a8-4eac-8ed8-ab58df40480f"
                     :dispatched-at #inst "2025-06-11T08:09:12"
                     :actions [[:transient/assoc-in [:syncing "task-id"] true]
                               (:action add-task-action)]}
     :dispatches [#uuid "81425764-6219-43f1-aa61-07110ea16fca"]
     :dispatched-at #inst "2025-06-11T08:09:13"
     :dispatch-elapsed (inspector/->timing 65)
     :dispatch-data {:number 42}
     :effects [{:effect [:state/assoc-in [:transient :syncing "task-id"] false]
                :effect-elapsed {:ms 1.0 :slow? false}
                :state {:tasks []
                        :transient {:syncing {"task-id" true}}}
                :result {:tasks []
                         :transient {:syncing {"task-id" false}}}}]
     :actions
     [{:action [:transient/assoc-in [:syncing "task-id"] false]
       :expansions [{:effect [:state/assoc-in [:transient :syncing "task-id"] false]
                     :effect-elapsed {:ms 1.0 :slow? false}
                     :state {:tasks []
                             :transient {:syncing {"task-id" true}}}}]
       :expansion-elapsed (inspector/->timing 16)
       :state {:tasks []
               :transient {:syncing {"task-id" true}}}}]}}})

(defscene action-list
  (hh/render-panel log))

(defscene dispatch-details
  (hh/navigate-to log [1]))

(defscene nested-dispatch
  (hh/navigate-to log [0]))

(defscene event-details
  (hh/navigate-to log [1 1]))

(defscene dispatch-data
  (hh/navigate-to log [1 2]))

(defscene action-detail
  (hh/navigate-to log [1 4]))

(defscene action-detail-action
  (hh/navigate-to log [1 4 0]))

(defscene action-detail-interpolations
  (hh/navigate-to log [1 4 1]))

(defscene action-detail-nested-action
  (hh/navigate-to log [1 4 5]))

(defscene action-detail-nested-dispatch
  (hh/navigate-to log [1 8]))

(defscene direct-effect-dispatch
  (hh/render-panel
   {:slow-threshold 100
    :entries
    {#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
     {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
      :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
      :actions
      [{:state {:number 0}
        :effect [:effects/save [:a] 1]
        :result {:number 0, :a 1}
        :effect-elapsed {:ms 1.0, :slow? false}}
       {:state {:number 0, :a 1}
        :effect [:effects/save [:b] 2]
        :result {:number 0, :a 1, :b 2}
        :effect-elapsed {:ms 1.0, :slow? false}}
       {:state {:number 0, :a 1, :b 2}
        :effect [:effects/save [:c] 3]
        :result {:number 0, :a 1, :b 2, :c 3}
        :effect-elapsed {:ms 1.0, :slow? false}}
       {:state {:number 0, :a 1, :b 2, :c 3}
        :effect [:effects/save [:d] 4]
        :result {:number 0, :a 1, :b 2, :c 3, :d 4}
        :effect-elapsed {:ms 1.0, :slow? false}}]
      :dispatch-data nil
      :effects
      [{:state {:number 0}
        :effect [:effects/save [:a] 1]
        :result {:number 0, :a 1}
        :effect-elapsed {:ms 1.0, :slow? false}}
       {:state {:number 0, :a 1}
        :effect [:effects/save [:b] 2]
        :result {:number 0, :a 1, :b 2}
        :effect-elapsed {:ms 1.0, :slow? false}}
       {:state {:number 0, :a 1, :b 2}
        :effect [:effects/save [:c] 3]
        :result {:number 0, :a 1, :b 2, :c 3}
        :effect-elapsed {:ms 1.0, :slow? false}}
       {:state {:number 0, :a 1, :b 2, :c 3}
        :effect [:effects/save [:d] 4]
        :result {:number 0, :a 1, :b 2, :c 3, :d 4}
        :effect-elapsed {:ms 1.0, :slow? false}}]
      :dispatch-elapsed {:ms 9.0, :slow? false}}}
    :chronology [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"]
    :now #inst "2026-06-03T08:42:00.000-00:00"}))

(def batched-dispatch
  {:entries
   {#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
    {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
     :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
     :actions [{:state {:number 0}
                :effects
                [[:effects/save-batch :a 1]
                 [:effects/save-batch :b 2]
                 [:effects/save-batch :c 3]
                 [:effects/save-batch :d 4]]
                :result {:number 0
                         :a 1
                         :b 2
                         :c 3
                         :d 4}
                :effect-elapsed {:ms 1.0, :slow? false}}]
     :dispatch-data nil
     :dispatch-elapsed {:ms 1.0, :slow? false}
     :effects [{:state {:number 0}
                :effects
                [[:effects/save-batch :a 1]
                 [:effects/save-batch :b 2]
                 [:effects/save-batch :c 3]
                 [:effects/save-batch :d 4]]
                :result {:number 0
                         :a 1
                         :b 2
                         :c 3
                         :d 4}
                :effect-elapsed {:ms 1.0, :slow? false}}]}}
   :chronology [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"]
   :now #inst "2026-06-03T08:42:00.000-00:00"})

(defscene batched-dispatch-listing
  (hh/render-panel batched-dispatch))

(defscene batched-dispatch-detail
  (hh/navigate-to batched-dispatch [0]))

(defscene batched-dispatch-detail-actions
  (hh/navigate-to batched-dispatch [0 2]))
