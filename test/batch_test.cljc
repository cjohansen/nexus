(ns batch-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus.batch :as batch]
            [nexus.core :as nexus]))

(def test-nexus
  {:nexus/system->state deref
   :nexus/effects
   {:effects/save
    ^:nexus/batch
    (fn [_ system path-vs]
      (swap! system
             (fn [state]
               (reduce (fn [s [p v]] (assoc-in s p v)) state path-vs))))}})

(deftest batch-test
  (testing "Batch processes effects"
    (is (= (-> (batch/install test-nexus)
               (nexus/dispatch (atom {:existing "Data"}) {}
                   [[:effects/save [:number] 3]
                    [:effects/save [:name] "Nexus"]]))
           {:results
            [{:effects [[:effects/save [:number] 3]
                        [:effects/save [:name] "Nexus"]]
              :res {:existing "Data"
                    :number 3
                    :name "Nexus"}}]})))

  (testing "Executes effects as close to dispatch order as possible, starting with unbatched ones"
    (is (= (let [log (atom [])]
             (-> {:nexus/effects
                  {:effects/save ^:nexus/batch
                   (fn [_ _ effect-args]
                     (doseq [args effect-args]
                       (swap! log conj (into [:effects/save] args))))

                   :effects/transact ^:nexus/batch
                   (fn [_ _ effect-args]
                     (doseq [args effect-args]
                       (swap! log conj (into [:effects/transact] args))))

                   :effects/alert
                   (fn [_ _ text]
                     (swap! log conj [:effects/alert text]))}}
                 batch/install
                 (nexus/dispatch (atom {}) {}
                     [[:effects/save :a 1]
                      [:effects/transact :A 1]
                      [:effects/alert "First"]
                      [:effects/save :b 2]
                      [:effects/alert "Second"]
                      [:effects/transact :B 2]]))
             @log)
           [[:effects/alert "First"]
            [:effects/alert "Second"]
            [:effects/save :a 1]
            [:effects/save :b 2]
            [:effects/transact :A 1]
            [:effects/transact :B 2]]))))
