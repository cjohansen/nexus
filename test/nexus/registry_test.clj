(ns nexus.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus.registry :as nxr]))

(nxr/register-system->state!
 (fn [store]
   (assoc @store :now #inst "2025-06-23T12:00:00Z")))

(nxr/register-action! ::start
  (fn [_ now]
    [[::save [:received-at] now]]))

(nxr/register-placeholder! :clock/now
  (fn [_]
    #inst "2025-06-23T12:00:00Z"))

(nxr/register-effect! ::save
  (fn [_ store path v]
    (swap! store assoc-in path v)))

(deftest registry-test
  (testing "Dispatches action via registry"
    (is (= (let [store (atom {})]
             (nxr/dispatch store {} [[::start [:clock/now]]]))
           {:results
            [{:effect [::save [:received-at] #inst "2025-06-23T12:00:00.000-00:00"]
              :res {:received-at #inst "2025-06-23T12:00:00.000-00:00"}}]}))))
