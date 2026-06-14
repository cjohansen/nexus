(ns nexus.inspector-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [nexus.action-log :as action-log]
            [nexus.core :as nexus]
            [nexus.inspector :as inspector]))

(def nexus
  {:nexus/system->state deref

   :nexus/actions
   {:actions/inc
    (fn [_ path]
      [[:actions/plus path 1]])

    :actions/plus
    (fn [state path n]
      (let [curr (get-in state path 0)]
        [[:effects/save path (+ curr n)]
         [:effects/save [:old path] curr]]))}

   :nexus/effects
   {:effects/save
    (fn [_ store k v]
      (swap! store assoc-in k v))

    :effects/save-batch
    ^:nexus/batch
    (fn [_ store kvs]
      (swap! store #(reduce (fn [s [k v]] (assoc s k v)) % kvs)))

    ;; I don't suggest anyone do this in a production app 😅
    ;; Somehow we gotta test this stuff!
    :effects/dispatch
    (fn [{:keys [dispatch]} store actions]
      (dispatch actions))}

   :nexus/placeholders
   {:secret/number
    (fn [{:keys [num]}]
      num)}})

(defn sequentially [ids]
  (let [ids! (atom ids)]
    (fn [& _]
      (ffirst (swap-vals! ids! next)))))

(defn inspect [nexus initial-state & [action-log-opts]]
  (let [log (action-log/create-log action-log-opts)]
    {:nexus (action-log/install-logger nexus log)
     :log log
     :store (atom (or initial-state {}))}))

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

(deftest logger-test
  (testing "Logs action at dispatch"
    (is (= (let [now (atom 0)]
             (with-redefs [inspector/now (constantly #inst "2026-06-03T08:40")
                           inspector/now-ms #(float (swap! now inc))
                           clojure.core/random-uuid (constantly #uuid "5efb659e-62b8-48d9-858c-813ebaad947b")]
               (let [{:keys [log nexus store]} (inspect nexus {:number 2})]
                 (nexus/dispatch nexus store {:num 42} [[:actions/plus [:number] [:secret/number]]])
                 (select-keys (datafy @log) [:chronology :entries]))))
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
                         :expansions [{:effect [:effects/save [:number] 44]
                                       :result {:number 44}
                                       :state {:number 2}
                                       :effect-elapsed {:ms 1.0, :slow? false}}
                                      {:effect [:effects/save [:old [:number]] 2]
                                       :result {:number 44, :old {[:number] 2}}
                                       :state {:number 44}
                                       :effect-elapsed {:ms 1.0, :slow? false}}]
                         :expansion-elapsed {:ms 5.0, :slow? false}}]
              :effects [{:effect [:effects/save [:number] 44]
                         :result {:number 44}
                         :state {:number 2}
                         :effect-elapsed {:ms 1.0, :slow? false}}
                        {:effect [:effects/save [:old [:number]] 2]
                         :result {:number 44, :old {[:number] 2}}
                         :state {:number 44}
                         :effect-elapsed {:ms 1.0, :slow? false}}]
              :dispatch-data {:num 42}
              :dispatch-elapsed {:ms 7.0, :slow? false}}}})))

  (testing "Nests action expansions"
    (is (= (-> (let [now (atom 0)]
                 (with-redefs [inspector/now (constantly #inst "2026-06-03T08:40")
                               inspector/now-ms #(float (swap! now inc))
                               clojure.core/random-uuid (constantly #uuid "5efb659e-62b8-48d9-858c-813ebaad947b")]
                   (let [{:keys [log nexus store]} (inspect nexus {:number 5})]
                     (nexus/dispatch nexus store {:num 42} [[:actions/inc [:number]]])
                     (datafy @log))))
               :entries)
           {#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
            {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
             :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
             :dispatch-data {:num 42}
             :dispatch-elapsed {:ms 9.0, :slow? false}
             :actions
             [{:action [:actions/inc [:number]]
               :state {:number 5}
               :expansions
               [{:action [:actions/plus [:number] 1]
                 :state {:number 5}
                 :expansions
                 [{:effect [:effects/save [:number] 6]
                   :state {:number 5}
                   :result {:number 6}
                   :effect-elapsed {:ms 1.0, :slow? false}}
                  {:effect [:effects/save [:old [:number]] 5]
                   :state {:number 6}
                   :result {:number 6, :old {[:number] 5}}
                   :effect-elapsed {:ms 1.0, :slow? false}}]
                 :expansion-elapsed {:ms 5.0, :slow? false}}]
               :expansion-elapsed {:ms 6.0, :slow? false}}]
             :effects
             [{:effect [:effects/save [:number] 6]
               :state {:number 5}
               :result {:number 6}
               :effect-elapsed {:ms 1.0, :slow? false}}
              {:effect [:effects/save [:old [:number]] 5]
               :state {:number 6}
               :result {:number 6, :old {[:number] 5}},
               :effect-elapsed {:ms 1.0, :slow? false}}]}})))

  (testing "Marks dispatch as slow according to config"
    (is (= (->> (let [now (atom 0)]
                  (with-redefs [inspector/now (constantly #inst "2026-06-03T08:40")
                                inspector/now-ms #(float (swap! now inc))
                                clojure.core/random-uuid (constantly #uuid "5efb659e-62b8-48d9-858c-813ebaad947b")]
                    (let [{:keys [log nexus store]} (inspect nexus {:number 5} {:slow-threshold 5})]
                      (nexus/dispatch nexus store {:num 42} [[:actions/inc [:number]]])
                      (datafy @log))))
                :entries
                vals
                first
                :dispatch-elapsed)
           {:ms 9.0, :slow? true})))

  (testing "Dispatches multiple actions"
    (is (= (->> (let [now (atom 0)]
                  (with-redefs [inspector/now (constantly #inst "2026-06-03T08:40")
                                inspector/now-ms #(float (swap! now inc))
                                clojure.core/random-uuid (sequentially [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
                                                                        #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])]
                    (let [{:keys [log nexus store]} (inspect nexus {:number 5})]
                      (nexus/dispatch nexus store {:num 42}
                        [[:actions/inc [:number]]
                         [:actions/inc [:number]]])
                      (nexus/dispatch nexus store {:num 42}
                        [[:actions/inc [:number]]
                         [:actions/inc [:number]]])
                      (datafy @log))))
                :entries
                vals
                count)
           2)))

  (testing "Adds dispatched-by"
    (is (= (-> (let [now (atom 0)]
                 (with-redefs [inspector/now (sequentially [#inst "2026-06-03T08:40"
                                                            #inst "2026-06-03T08:41"])
                               inspector/now-ms #(float (swap! now inc))
                               clojure.core/random-uuid (sequentially [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
                                                                       #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])]
                   (let [{:keys [log nexus store]} (inspect nexus {:number 5})]
                     (nexus/dispatch nexus store {:num 42}
                       [[:actions/inc [:number]]
                        [:effects/dispatch
                         [[:actions/inc [:number]]]]])
                     (datafy @log))))
               (get-in [:entries #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])
               :dispatched-by)
           {:id #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
            :dispatched-at #inst "2026-06-03T08:40:00.000-00:00"
            :actions [[:actions/inc [:number]]
                      [:effects/dispatch [[:actions/inc [:number]]]]]})))

  (testing "Marks nested dispatches"
    (is (= (-> (let [now (atom 0)]
                 (with-redefs [inspector/now (sequentially [#inst "2026-06-03T08:40"
                                                            #inst "2026-06-03T08:41"])
                               inspector/now-ms #(float (swap! now inc))
                               clojure.core/random-uuid (sequentially [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
                                                                       #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])]
                   (let [{:keys [log nexus store]} (inspect nexus {:number 5})]
                     (nexus/dispatch nexus store {:num 42}
                       [[:actions/inc [:number]]
                        [:effects/dispatch
                         [[:actions/inc [:number]]]]])
                     (datafy @log))))
               (get-in [:entries #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"])
               :dispatches)
           [#uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])))

  (testing "Adds nested dispatch to originating effect"
    (is (= (-> (let [now (atom 0)]
                 (with-redefs [inspector/now (sequentially [#inst "2026-06-03T08:40"
                                                            #inst "2026-06-03T08:41"])
                               inspector/now-ms #(float (swap! now inc))
                               clojure.core/random-uuid (sequentially [#uuid "5efb659e-62b8-48d9-858c-813ebaad947b"
                                                                       #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"])]
                   (let [{:keys [log nexus store]} (inspect nexus {:number 5})]
                     (nexus/dispatch nexus store {:num 42}
                       [[:actions/inc [:number]]
                        [:effects/dispatch
                         [[:actions/inc [:number]]]]])
                     (datafy @log))))
               (get-in [:entries #uuid "5efb659e-62b8-48d9-858c-813ebaad947b"])
               :actions
               second
               :dispatches)
           [{:id #uuid "3228a005-6ff1-44d4-97c1-71bb7cdb4820"
             :dispatched-at #inst "2026-06-03T08:41:00.000-00:00"
             :actions [[:actions/inc [:number]]]}]))))
