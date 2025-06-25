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
    (fn [_ n]
      [[:effects/save [:number] (inc n)]])}

   :nexus/effects
   {:effects/save
    (fn [_ store k v]
      (swap! store assoc k v))

    :effects/save-batch
    ^:nexus/batch
    (fn [_ store kvs]
      (swap! store #(reduce (fn [s [k v]] (assoc s k v)) % kvs)))}

   :nexus/placeholders
   {:secret/number
    (fn [{:keys [num]}]
      num)}})

(defn inspect [nexus]
  (let [log (action-log/create-log)]
    {:nexus (action-log/install-logger nexus log)
     :log log
     :store (atom {})}))

(defn datafy [data]
  (walk/postwalk
   (fn [x]
     (cond-> x
       (and (:data x) (:f x)) :data))
   data))

(defn fake-clock []
  (let [now (atom 0)]
    (fn []
      (swap! now inc))))

(deftest logger-test
  (testing "Logs action at dispatch"
    (is (= (let [now (atom 0)]
             (with-redefs [inspector/now (constantly #inst "2025-06-13T08:40")
                           inspector/now-ms #(float (swap! now inc))]
               (let [{:keys [log nexus store]} (inspect nexus)]
                 (nexus/dispatch nexus store {:data 42} [[:actions/inc 2]])
                 (datafy @log))))
           [{:dispatched-at #inst "2025-06-13T08:40:00.000-00:00"
             :actions [[:actions/inc 2]]
             :dispatch-data {:data 42}
             :state {}
             :expansions [{:action [:actions/inc 2]
                           :expansion [[:effects/save [:number] 3]]}]
             :effects [[:effects/save [:number] 3]]
             :results [{:effect [:effects/save [:number] 3]
                        :res {[:number] 3}}]
             :dispatch-elapsed 1.0}])))

  (testing "Stores dispatches chronologically"
    (is (= (with-redefs [inspector/now (constantly #inst "2025-06-13T08:40")]
             (let [{:keys [log nexus store]} (inspect nexus)]
               (nexus/dispatch nexus store {:data 42} [[:actions/inc 2]])
               (nexus/dispatch nexus store {:data 42} [[:actions/inc 3]])
               (map :actions (datafy @log))))
           [[[:actions/inc 2]]
            [[:actions/inc 3]]])))

  (testing "Logs actions before and after placeholder interpolation"
    (is (= (let [{:keys [log nexus store]} (inspect nexus)]
             (nexus/dispatch nexus store {:num 42} [[:actions/inc [:secret/number]]])
             (:expansions (first (datafy @log))))
           [{:action [:actions/inc [:secret/number]]
             :expansion [[:effects/save [:number] 43]]}])))

  (testing "Logs dispatched effect"
    (is (= (let [{:keys [log nexus store]} (inspect nexus)]
             (nexus/dispatch nexus store {:num 42} [[:effects/save [:number] [:secret/number]]])
             (-> (datafy @log)
                 first
                 :expansions))
           [{:action [:effects/save [:number] [:secret/number]]
             :expansion [:effects/save [:number] 42]}])))

  (testing "Logs dispatched batched effect"
    (is (= (let [{:keys [log nexus store]} (inspect nexus)]
             (nexus/dispatch nexus store {:num 42}
               [[:effects/save-batch [:number] [:secret/number]]
                [:effects/save-batch [:name] "Nexus"]])
             (-> (datafy @log)
                 first
                 (select-keys [:actions :expansions])))
           {:actions [[:effects/save-batch [:number] [:secret/number]]
                      [:effects/save-batch [:name] "Nexus"]]
            :expansions [{:action [:effects/save-batch [:number] [:secret/number]]
                          :expansion [:effects/save-batch [:number] 42]}]}))))
