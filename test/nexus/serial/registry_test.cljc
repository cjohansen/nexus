(ns nexus.serial.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus.serial.registry :as nxsr]))

(nxsr/register-system->state!
 (fn [store]
   (assoc @store :now #inst "2026-02-23T12:00:00Z")))

(nxsr/register-action! ::start
  (fn [_ now]
    [[::save [:received-at] now]]))

(nxsr/register-action! ::double-check
  (fn [state]
    [[::save [:control] (:received-at state)]]))

(nxsr/register-placeholder! :clock/now
  (fn [_]
    #inst "2026-02-23T12:00:00Z"))

(nxsr/register-effect! ::save
  (fn [_ store path v]
    (swap! store assoc-in path v)))

(deftest registry-test
  (testing "Dispatches action via registry"
    (is (= (let [store (atom {})]
             (nxsr/dispatch store {} [[::start [:clock/now]]
                                      [::double-check]]))
           {:results
            [{:effect [::save [:received-at] #inst "2026-02-23T12:00:00.000-00:00"]
              :res {:received-at #inst "2026-02-23T12:00:00.000-00:00"}}
             {:effect [::save [:control] #inst "2026-02-23T12:00:00.000-00:00"]
              :res {:received-at #inst "2026-02-23T12:00:00.000-00:00"
                    :control #inst "2026-02-23T12:00:00.000-00:00"}}]}))))
