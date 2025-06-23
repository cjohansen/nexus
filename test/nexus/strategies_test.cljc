(ns nexus.strategies-test
  (:require [nexus.strategies :as strategies]
            [nexus.test-helper :as h]
            [clojure.test :refer [deftest is testing]]
            [nexus.core :as nexus]))

(def nexus
  {:system->state deref

   :actions
   {:actions/fail
    (fn [_ _]
      (throw (ex-info "Boom!" {})))

    :actions/succeed
    (fn [_ arg]
      [[:effects/succeed arg]])}

   :effects
   {:effects/fail
    (fn [_ _ _]
      (throw (ex-info "Boom!" {})))

    :effects/succeed
    (fn [_ _ arg]
      arg)}})

(deftest fail-fast-test
  (testing "Aborts on first action error"
    (is (= (-> nexus
               (update :interceptors conj strategies/fail-fast)
               (nexus/dispatch (atom {}) {}
                   [[:actions/fail 1] ;; Expansion fails
                    [:actions/succeed 2]
                    [:effects/fail 3]]) ;; ...so this won't be executed
               h/datafy-errors)
           {:errors [{:phase :execute-effect
                      :effect [:effects/fail 3]
                      :err {:message "Boom!"
                            :data {}}}]})))

  (testing "Aborts on first effect error"
    (is (= (-> nexus
               (update :interceptors conj strategies/fail-fast)
               (nexus/dispatch (atom {}) {}
                   [[:actions/succeed 2]
                    [:effects/fail 3]])
               h/datafy-errors)
           {:errors [{:phase :execute-effect
                      :effect [:effects/fail 3]
                      :err {:message "Boom!"
                            :data {}}}]}))))
