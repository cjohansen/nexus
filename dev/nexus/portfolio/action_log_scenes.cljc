(ns nexus.portfolio.action-log-scenes
  (:require [dataspex.panel :as panel]
            [dataspex.ui :as ui]
            [lookup.core :as lookup]
            [nexus.inspector :as inspector]
            [portfolio.replicant :refer [defscene]]))

(def add-task-tx
  {:action [:db/save {:task/id "task-id"
                      :task/title "Do chores"}]
   :effects [{:effects [[:db/transact
                         [{:task/id "task-id"
                           :task/title "Do chores"}]]]
              :state {:tasks []
                      :transient {:syncing {"task-id" true}}}
              :result [1 :form/id "form" 536870913 true]}]})

(def add-task-action
  {:action [:task/add "task-id" {:task/title [:input/value]}]
   :interpolated [:task/add "task-id" {:task/title "Do chores"}]
   :interpolations {[:input/value] "Do chores"}
   :interpolation-elapsed (inspector/->timing 12)
   :expansion-elapsed (inspector/->timing 23)
   :expansions [{:action [:dev/log :create-task "task-id" "Do chores"]
                 :effects {:effects [[:dev/log :create-task "task-id" "Do chores"]]}
                 :state {:tasks []
                         :transient {:syncing {"task-id" true}}}
                 :result nil}
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
                :effects
                {:effects [[:actions/execute-command
                            {:command/data {:person/tiltalenavn "Christian"}
                             :command/kind :commands/sett-mitt-tiltalenavn}]]}}]}

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
     :effects [{:effects [(dissoc add-task-tx :action)]}]
     :errors [{}]}

    #uuid "81425764-6219-43f1-aa61-07110ea16fca"
    {:id #uuid "81425764-6219-43f1-aa61-07110ea16fca"
     :dispatched-at #inst "2025-06-11T08:07:23"
     :dispatch-elapsed (inspector/->timing 12)
     :actions [add-task-action]
     :effects [{:effects [(dissoc add-task-tx :action)]}]}

    #uuid "29e14b68-a5a8-4eac-8ed8-ab58df40480f"
    {:id #uuid "29e14b68-a5a8-4eac-8ed8-ab58df40480f"
     :dispatched-at #inst "2025-06-11T08:09:12"
     :dispatch-elapsed (inspector/->timing 233 {:slow? true})
     :dispatch-data {:number 42}
     :effects (concat
               [{:effects [[:state/assoc-in [:transient :syncing "task-id"] true]]
                 :state {:tasks []}
                 :result {:tasks []
                          :transient {:syncing {"task-id" true}}}
                 :dispatches [{:id #uuid "2b105ea8-99cc-4d7b-aabf-fc2568c56d0a"
                               :dispatched-at #inst "2025-06-11T08:09:13"
                               :actions [[:transient/assoc-in [:syncing "task-id"] false]]}]}
                {:effects [[:dev/log :create-task "task-id" "Do chores"]]
                 :state {:tasks []
                         :transient {:syncing {"task-id" true}}}
                 :result nil}]
               (:effects add-task-tx))
     :actions
     [{:action [:transient/assoc-in [:syncing "task-id"] true]
       :expansions [{:effects {:effects [[:state/assoc-in [:transient :syncing "task-id"] true]]}
                     :state {:tasks []}}]
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
     :dispatched-at #inst "2025-06-11T08:09:13"
     :dispatch-elapsed (inspector/->timing 65)
     :dispatch-data {:number 42}
     :effects [{:effects [[:state/assoc-in [:transient :syncing "task-id"] false]]
                :state {:tasks []
                        :transient {:syncing {"task-id" true}}}
                :result {:tasks []
                         :transient {:syncing {"task-id" false}}}}]
     :actions
     [{:action [:transient/assoc-in [:syncing "task-id"] false]
       :expansions [{:effects [[:state/assoc-in [:transient :syncing "task-id"] false]]
                     :state {:tasks []
                             :transient {:syncing {"task-id" true}}}}]
       :expansion-elapsed (inspector/->timing 16)
       :state {:tasks []
               :transient {:syncing {"task-id" true}}}}]}}})

(defn render-panel [log & [path]]
  (panel/render-inspector
   {"Actions"
    {:dataspex/path (or path [])
     :dataspex/activity :dataspex.activity/browse
     :dataspex/inspectee "Actions"
     :dataspex/auditable? false
     :val (inspector/->LogInspector log)}}))

(defn find-navigate-path [dataspex-entry]
  (->> dataspex-entry
       lookup/attrs
       ::ui/actions
       (filter (comp #{:dataspex.actions/navigate} first))
       first
       last))

(defn navigate-to [log path]
  (->> path
       (reduce
        (fn [dataspex-path n]
          (->> (render-panel log dataspex-path)
               (lookup/select [:dataspex.ui/entry])
               (drop n)
               first
               find-navigate-path))
        [])
       (render-panel log)))

(defscene action-list
  (render-panel log))

(defscene dispatch-details
  (navigate-to log [1]))

(defscene nested-dispatch
  (navigate-to log [0]))

(defscene event-details
  (navigate-to log [1 1]))

(defscene dispatch-data
  (navigate-to log [1 2]))

(defscene action-detail
  (navigate-to log [1 4]))

(defscene action-detail-action
  (navigate-to log [1 4 0]))

(defscene action-detail-interpolations
  (navigate-to log [1 4 1]))

(defscene action-detail-nested-action
  (navigate-to log [1 4 5]))

(defscene action-detail-nested-dispatch
  (navigate-to log [1 8]))
