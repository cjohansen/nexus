(ns nexus.inspector-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [dataspex.hiccup :as hiccup]
            [lookup.core :as lookup]
            [nexus.action-log :as action-log]
            [nexus.batching :as batching]
            [nexus.core :as nexus]
            [nexus.inspector :as inspector])
  #?(:clj (:import (java.util Date))))

(def nexus
  (-> {:nexus/system->state deref

       :nexus/actions
       {:actions/inc
        (fn [_ path]
          [[:actions/plus path 1]])

        :actions/plus
        (fn [state path n]
          (let [curr (get-in state path 0)]
            [[:effects/save path (+ curr n)]
             [:effects/save [:old path] curr]]))

        :actions/noop
        (fn [_state])}

       :nexus/effects
       {:effects/save
        (fn [_ system k v]
          (swap! system assoc-in k v))

        :effects/save-batch
        ^:nexus/batch
        (fn [_ system kvs]
          (swap! system #(reduce (fn [s [k v]] (assoc s k v)) % kvs)))

        ;; I don't suggest anyone do this in a production app 😅
        ;; Somehow we gotta test this stuff!
        :effects/dispatch
        (fn [{:keys [dispatch]} _ actions]
          (dispatch actions))}

       :nexus/placeholders
       {:secret/number
        (fn [{:keys [num]}]
          num)}}
      batching/install))

(defn sequentially [ids]
  (let [ids! (atom ids)]
    (fn [& _]
      (ffirst (swap-vals! ids! next)))))

(defn inspect [nexus initial-state & [action-log-opts]]
  (let [log (action-log/create-log action-log-opts)]
    {:nexus (action-log/install-logger nexus log)
     :log log
     :system (atom (or initial-state {}))}))

(defn datafy [data]
  (walk/postwalk
   (fn [x]
     (cond->> x
       (and (:data x) (:f x)) :data
       (and (:ms x) (contains? x :slow?)) (into {})))
   data))

(defn fake-clock []
  (let [now (atom 0)]
    (fn []
      (swap! now inc))))

(defn tick [date]
  (#?(:clj Date. :cljs js/Date.) (+ (.getTime date) (* 60 1000))))

(def ids
  [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
   #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"
   #uuid "ec1a7cd8-9d7b-4e62-a39a-09ff60ac0975"
   #uuid "cd4f965d-c0bf-4f43-b5b1-9cc703f7d218"
   #uuid "62a2e15d-877e-4db9-96ce-f1d0c6c1c745"
   #uuid "09ca3f94-c095-4ebf-aa2c-5068db403867"
   #uuid "2c311575-fce5-453d-91d2-fe97530e3577"
   #uuid "22631f75-a8bf-4c7e-ac6d-1b2b49bc51cf"
   #uuid "92b4a9c9-06cc-4064-9353-b537524cc7df"
   #uuid "40456529-599f-4fec-be5a-e630df35e0ca"
   #uuid "2f8eac06-21a2-4587-adc7-9348605fa187"
   #uuid "fff8a807-90e2-48d5-bfa1-42bdc0c06737"
   #uuid "2615c909-7b20-4205-9185-f40e37925a11"
   #uuid "8eccc4cb-2b0d-488d-9c66-6526a37d6284"
   #uuid "32088666-1cb9-49ca-bd62-d7f8e6ae0e31"
   #uuid "53c0ccaa-fbff-4d4a-802f-7abf0fe64b79"
   #uuid "dd3e3ec9-85d6-4d02-8386-5a2586f14630"
   #uuid "2a946dac-a8ff-4f0f-aa35-57403e4d2ae1"
   #uuid "be3e24a4-e542-4cf1-8adc-a6fadc608b50"
   #uuid "ccd78c42-1954-45ab-a54a-ff495ab5f7db"
   #uuid "b3183659-5fad-42d4-9969-f90d547f3a71"
   #uuid "2469bc16-600b-45b5-8155-93ab8deab7ae"
   #uuid "95fe8041-2153-48d8-af8b-8996818f06d0"
   #uuid "b775fa63-7190-49a9-8baf-0a825f770d34"
   #uuid "86c16d9f-9726-4d91-9999-1469eede3909"
   #uuid "a52872cf-012b-41e2-a1c5-193f34a89a04"
   #uuid "777915c8-2d31-46fe-8a64-4ea452a72c48"
   #uuid "05351867-5550-4066-b9c6-2e65c1ede77d"])

(defn make-dispatcher [initial-state & [opt]]
  (let [now-ms (atom 0)
        now (atom #inst "2026-06-03T08:39")
        get-now #(swap! now tick)
        make-random-uuid (sequentially ids)
        {:keys [log nexus system]} (inspect nexus initial-state opt)]
    (fn dispatch
      ([actions]
       (dispatch nil actions))
      ([dispatch-data actions]
       (with-redefs [inspector/now get-now
                     inspector/now-ms #(float (swap! now-ms inc))
                     clojure.core/random-uuid make-random-uuid]
         (nexus/dispatch nexus system dispatch-data actions)
         (assoc (datafy @log) :now (get-now)))))))

(defn dispatch-actions
  ([actions initial-state]
   (dispatch-actions actions initial-state {}))
  ([actions initial-state dispatch-data]
   (let [dispatch (make-dispatcher initial-state)]
     (dispatch dispatch-data actions))))

(deftest logger-test
  (testing "Logs action at dispatch"
    (is (= (-> [[:actions/plus [:number] [:secret/number]]]
               (dispatch-actions {:number 2} {:num 42})
               (select-keys [:chronology :entries]))
           {:chronology '(#uuid "5efb659e-62b8-48d9-858c-813ebaad947b")
            :entries
            {#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
             {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
              :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
              :actions [{:action [:actions/plus [:number] [:secret/number]]
                         :interpolated [:actions/plus [:number] 42]
                         :interpolations {[:secret/number] 42}
                         :interpolation-elapsed {:ms 1.0, :slow? false}
                         :state {:number 2}
                         :expansions [{:state {:number 2}
                                       :action [:effects/save [:number] 44]
                                       :result {:number 44}
                                       :effect-elapsed {:ms 1.0, :slow? false}}
                                      {:action [:effects/save [:old [:number]] 2]
                                       :result {:number 44, :old {[:number] 2}}
                                       :state {:number 44}
                                       :effect-elapsed {:ms 1.0, :slow? false}}]
                         :expansion-elapsed {:ms 4.0, :slow? false}}]
              :effects [[:actions 0 :expansions 0]
                        [:actions 0 :expansions 1]]
              :dispatch-data {:num 42}
              :dispatch-elapsed {:ms 11.0, :slow? false}}}})))

  (testing "Nests action expansions"
    (is (= (-> [[:actions/inc [:number]]]
               (dispatch-actions {:number 5} {:num 42})
               :entries)
           {#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
            {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
             :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
             :dispatch-data {:num 42}
             :dispatch-elapsed {:ms 13.0, :slow? false}
             :actions
             [{:action [:actions/inc [:number]]
               :state {:number 5}
               :expansion-elapsed {:ms 5.0, :slow? false}
               :expansions
               [{:action [:actions/plus [:number] 1]
                 :state {:number 5}
                 :expansion-elapsed {:ms 4.0, :slow? false}
                 :expansions
                 [{:action [:effects/save [:number] 6]
                   :state {:number 5}
                   :result {:number 6}
                   :effect-elapsed {:ms 1.0, :slow? false}}
                  {:action [:effects/save [:old [:number]] 5]
                   :state {:number 6}
                   :result {:number 6, :old {[:number] 5}}
                   :effect-elapsed {:ms 1.0, :slow? false}}]}]}]
             :effects
             [[:actions 0 :expansions 0 :expansions 0]
              [:actions 0 :expansions 0 :expansions 1]]}})))

  (testing "Marks dispatch as slow according to config"
    (is (true? (-> (let [dispatch (make-dispatcher {:number 5} {:slow-threshold 5})]
                     (dispatch [[:actions/inc [:number]]]))
               :entries
               vals
               first
               :dispatch-elapsed
               :slow?))))

  (testing "Dispatches multiple actions"
    (is (= (-> (let [dispatch (make-dispatcher {:number 5})]
                 (dispatch [[:actions/inc [:number]]
                            [:actions/inc [:number]]])
                 (dispatch [[:actions/inc [:number]]
                            [:actions/inc [:number]]]))
               :entries
               vals
               count)
           2)))

  (testing "Adds dispatched-by"
    (is (= (-> [[:actions/inc [:number]]
                [:effects/dispatch
                 [[:actions/inc [:number]]]]]
               (dispatch-actions {:number 5})
               (get-in [:entries #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])
               :dispatched-by)
           {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
            :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
            :actions [[:actions/inc [:number]]
                      [:effects/dispatch [[:actions/inc [:number]]]]]})))

  (testing "Marks nested dispatches"
    (is (= (-> [[:actions/inc [:number]]
                [:effects/dispatch
                 [[:actions/inc [:number]]]]]
               (dispatch-actions {:number 5})
               (get-in [:entries #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"])
               :dispatches)
           [#uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])))

  (testing "Adds nested dispatch to originating effect"
    (is (= (-> [[:actions/inc [:number]]
                [:effects/dispatch
                 [[:actions/inc [:number]]]]]
               (dispatch-actions {:number 5})
               (get-in [:entries #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"])
               :actions
               second
               :dispatches)
           [{:id #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"
             :dispatched-at #inst "2026-06-03T08:41:00.000-00:00"
             :actions [[:actions/inc [:number]]]}])))

  (testing "Truncates log to most recent dispatches"
    (is (= (->> (let [dispatch (make-dispatcher {:number 0} {:max-entries 3})]
                  (dispatch [[:actions/inc [:number]]])
                  (dispatch [[:actions/inc [:number]]])
                  (dispatch [[:actions/inc [:number]]])
                  (dispatch [[:actions/inc [:number]]])
                  (dispatch [[:actions/inc [:number]]]))
                inspector/get-action-log
                (mapv (comp :dispatched-at :k)))
           [#inst "2026-06-03T08:52:00.000-00:00"
            #inst "2026-06-03T08:49:00.000-00:00"
            #inst "2026-06-03T08:46:00.000-00:00"])))

  (testing "Truncates log by clock"
    (is (= (-> (let [dispatch (make-dispatcher {:number 0} {:max-age {:minutes 5}})]
                 (dispatch [[:actions/inc [:number]]])
                 (dispatch [[:actions/inc [:number]]])
                 (dispatch [[:actions/inc [:number]]])
                 (dispatch [[:actions/inc [:number]]])
                 (dispatch [[:actions/inc [:number]]]))
               inspector/get-action-log
               (->> (mapv (comp :dispatched-at :k))))
           [#inst "2026-06-03T08:52:00.000-00:00"
            #inst "2026-06-03T08:49:00.000-00:00"])))

  (testing "Understands batched effects"
    (is (= (->> (let [dispatch (make-dispatcher {:number 0})]
                  (dispatch [[:effects/save-batch :a 1]
                             [:effects/save-batch :b 2]
                             [:effects/save-batch :c 3]
                             [:effects/save-batch :d 4]]))
                :entries
                vals
                (map #(select-keys % [:actions :effects])))
           [{:actions
             [{:state {:number 0}
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
             :effects
             [[:actions 0]]}])))

  (testing "Tolerates noop actions"
    (is (= (-> [[:actions/noop]]
               (dispatch-actions {:number 2} {:num 42})
               (select-keys [:chronology :entries])
               :entries
               vals
               first
               (select-keys [:actions :effects]))
           {:actions
            [{:action [:actions/noop]
              :state {:number 2}}]})))

  (testing "Dispatches effect directly"
    (is (= (-> [[:effects/save [:number] 2]]
               (dispatch-actions {} {})
               inspector/->LogInspector
               hiccup/render-dictionary
               (->> (lookup/select-one [:dataspex.ui/entry :dataspex.ui/source])
                    lookup/children
                    first))
           [:dataspex.ui/vector
            [:dataspex.ui/keyword :effects/save]
            [:dataspex.ui/vector [:dataspex.ui/keyword :number]]
            [:dataspex.ui/number 2]])))

  (testing "Prints actions in failed attempt"
    (is (= (-> [[:actions/non-existent]]
               (dispatch-actions {} {})
               inspector/->LogInspector
               hiccup/render-dictionary
               (->> (lookup/select-one [:dataspex.ui/entry :dataspex.ui/source])
                    lookup/children
                    first))
           [:dataspex.ui/vector
            [:dataspex.ui/keyword :actions/non-existent]])))
  )
