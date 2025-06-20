(ns nexus.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [nexus.core :as nexus]))

(deftest action?-test
  (testing "Detects action"
    (is (false? (nexus/action? :action)))
    (is (true? (nexus/action? [:action])))
    (is (false? (nexus/action? ["action"])))))

(deftest actions?-test
  (testing "Detects actions"
    (is (false? (nexus/actions? :action)))
    (is (false? (nexus/actions? [:action])))
    (is (false? (nexus/actions? [["action"]])))
    (is (true? (nexus/actions? [[:action]])))
    (is (true? (nexus/actions? [[:actions/doit "Now!"]])))
    (is (true? (nexus/actions? [[:actions/doit "Now!"]
                                [:actions/also "Do" :this]])))))

(defn get-message [e]
  #?(:clj (.getMessage e)
     :cljs (.-message e)))

(defn ex->data [e]
  {:message (get-message e)
   :data (walk/prewalk
          (fn [x]
            (cond
              (and (:err x) (not (map? (:err x))))
              (let [data (ex-data (:err x))]
                (assoc x :err
                       (cond-> {:message (get-message (:err x))}
                         (not-empty data) (assoc :data data))))

              (fn? x)
              ::fn

              :else x))
          (ex-data e))})

(defn datafy-errors [res]
  (update res :errors (fn [errors] (mapv #(update % :err ex->data) errors))))

(defn ^{:indent 2} with-interceptor [nexus phase f & [id]]
  (update nexus :interceptors (fnil conj []) (cond-> {phase f}
                                               id (assoc :id id))))

(def nexus-with-inc
  {:actions
   {:actions/inc
    (fn [state n]
      [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}})

(defn log-interceptor [log n]
  {:id n
   :before-action (fn [in]
                    (swap! log conj [:before-action n (get-in in [:action 0])])
                    in)
   :after-action (fn [in]
                   (swap! log conj (cond-> [:after-action n (get-in in [:action 0])]
                                     (seq (:actions in)) (conj (mapv first (:actions in)))))
                   in)
   :before-effect (fn [in]
                    (swap! log conj [:before-effect n
                                     (first (or (:effect in) (first (:effects in))))])
                    in)
   :after-effect (fn [in]
                   (swap! log conj [:after-effect n
                                    (first (or (:effect in) (first (:effects in))))
                                    (:res in)])
                   in)})

(deftest expand-actions-test
  (testing "Noops without any expansions"
    (is (= (->> [[:actions/test :it]]
                (nexus/expand-actions {} {})
                :effects)
           [[:actions/test :it]])))

  (testing "Expands action"
    (is (= (-> {:actions
                {:actions/test
                 (fn [_ arg]
                   [[:actions/store (str/upper-case arg)]])}}
               (nexus/expand-actions {} [[:actions/test "it"]])
               :effects)
           [[:actions/store "IT"]])))

  (testing "Passes state to action handler"
    (is (= (-> {:actions
                {:actions/test
                 (fn [state arg]
                   [[:actions/store (:n (:config state)) arg]])}}
               (nexus/expand-actions {:config {:n 2}} [[:actions/test "it"]])
               :effects)
           [[:actions/store 2 "it"]])))

  (testing "Returns error when action handler does not return collection of actions"
    (is (= (-> {:actions
                {:actions/test
                 (fn [{:keys [config]} arg]
                   [:actions/store (:n config) arg])}}
               (nexus/expand-actions {:config {:n 2}} [[:actions/test "it"]])
               datafy-errors)
           {:errors
            [{:action [:actions/test "it"]
              :phase :expand-action
              :err {:message ":actions/test should expand to a collection of actions"
                    :data {:res [:actions/store 2 "it"]}}}]})))

  (testing "Expands action to empty list of effects"
    (is (empty? (-> {:actions
                     {:actions/test
                      (fn [_ _] [])}}
                    (nexus/expand-actions {:config {:n 2}} [[:actions/test "it"]])
                    :effects))))

  (testing "Expands actions recursively"
    (is (= (-> {:actions
                {:actions/inc
                 (fn [_ n]
                   [[:actions/plus n 1]])
                 :actions/plus
                 (fn [_ a b]
                   [[:actions/store "n" (+ a b)]])}}
               (nexus/expand-actions {} [[:actions/inc 2]])
               :effects)
           [[:actions/store "n" 3]])))

  (testing "Returns errors from bad action handler"
    (is (= (-> {:actions
                {:actions/inc
                 (fn [_ _]
                   (throw (ex-info "Boom!" {})))}}
               (nexus/expand-actions {} [[:actions/inc 2]])
               datafy-errors)
           {:errors [{:phase :expand-action
                      :action [:actions/inc 2]
                      :err {:message "Boom!"
                            :data {}}}]})))

  (testing "Calls before-interceptor before action handler"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 #(assoc-in % [:state :base-n] 2))
               (nexus/expand-actions {} [[:actions/inc 9]]))
           {:effects [[:effects/save [:number] 12]]})))

  (testing "Calls after-interceptor after action handler"
    (is (= (let [log (atom [])]
             (-> (with-interceptor nexus-with-inc :after-action
                   (fn [context]
                     (swap! log conj {:in (:action context) :out (:actions context)})
                     context))
                 (nexus/expand-actions {} [[:actions/inc 9]]))
             @log)
           [{:in [:actions/inc 9]
             :out [[:effects/save [:number] 10]]}])))

  (testing "Does not call interceptors for actions that have no handlers"
    (is (= (let [log (atom [])]
             (-> {:interceptors
                  [{:before-action
                    (fn [ctx]
                      (swap! log conj ctx))}]}
                 (nexus/expand-actions {:state "Here"} [[:actions/inc 2]]))
             @log)
           [])))

  (testing "Returns error from before-action interceptor"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 (fn [ctx]
                   (throw (ex-info "Boom!" {:ctx ctx}))))
               (nexus/expand-actions {:state "Here"} [[:actions/inc 2]])
               datafy-errors)
           {:effects [[:effects/save [:number] 3]]
            :errors
            [{:phase :before-action
              :err
              {:message "Boom!"
               :data
               {:ctx
                {:state {:state "Here"}
                 :action [:actions/inc 2]
                 :queue [{:phase :expand-action
                          :before-action ::fn}]
                 :stack [{:before-action ::fn}]}}}
              :action [:actions/inc 2]}]})))

  (testing "Returns error from after-action interceptor"
    (is (= (-> (with-interceptor nexus-with-inc :after-action
                 (fn [ctx]
                   (throw (ex-info "Boom!" {:ctx ctx})))
                 :logger)
               (nexus/expand-actions {:state "Here"} [[:actions/inc 2]])
               datafy-errors)
           {:effects [[:effects/save [:number] 3]]
            :errors
            [{:phase :after-action
              :id :logger
              :err
              {:message "Boom!"
               :data
               {:ctx
                {:state {:state "Here"}
                 :action [:actions/inc 2]
                 :queue nil
                 :stack nil
                 :actions [[:effects/save [:number] 3]]}}}
              :action [:actions/inc 2]}]})))

  (testing "Allows interceptor to abort action expansion flow"
    (is (= (-> (with-interceptor nexus-with-inc :before-action
                 #(throw (ex-info "Boom!" {:ctx %}))
                 :logger)
               (with-interceptor :before-action
                 #(cond-> %
                    (:errors %) (dissoc :queue :stack))
                 :abort-early)
               (nexus/expand-actions {:state "Here"} [[:actions/inc 2]])
               datafy-errors
               (update-in [:errors 0 :err] select-keys [:message]))
           ;; No effects!
           {:errors
            [{:id :logger
              :phase :before-action
              :action [:actions/inc 2]
              :err {:message "Boom!"}}]})))

  (testing "Calls interceptors in order"
    (is (= (let [log (atom [])]
             (-> {:actions
                  {:actions/inc
                   (fn [state n]
                     (swap! log conj [:action])
                     [[:effects/save [:number] (+ (or (:base-n state) 0) n 1)]])}
                  :interceptors [(log-interceptor log 1)
                                 (log-interceptor log 2)
                                 (log-interceptor log 3)]}
                 (nexus/expand-actions {} [[:actions/inc 9]]))
             @log)
           [[:before-action 1 :actions/inc]
            [:before-action 2 :actions/inc]
            [:before-action 3 :actions/inc]
            [:action]
            [:after-action 3 :actions/inc [:effects/save]]
            [:after-action 2 :actions/inc [:effects/save]]
            [:after-action 1 :actions/inc [:effects/save]]]))))

(deftest interpolate-test
  (testing "No-ops when there are no placeholders"
    (is (= (nexus/interpolate {} {} [[:actions/inc 3]])
           [[:actions/inc 3]])))

  (testing "Replaces placeholder form"
    (is (= (-> {:placeholders {:number (fn [{:keys [value]}] value)}}
               (nexus/interpolate {:value 3} [[:actions/inc [:number]]]))
           [[:actions/inc 3]])))

  (testing "Does not replace naked placeholder keyword"
    (is (= (-> {:placeholders {:number (fn [{:keys [value]}] value)}}
               (nexus/interpolate {:value 3} [[:actions/inc :number]]))
           [[:actions/inc :number]])))

  (testing "Nests placeholders"
    (is (= (-> {:placeholders
                {:value (fn [{:keys [value]}] value)
                 :number (fn [_ s] (some-> s parse-long))}}
               (nexus/interpolate {:value "5"} [[:actions/inc [:number [:value]]]]))
           [[:actions/inc 5]]))))

(def nexus-with-save
  {:effects
   {:effects/save
    (fn [_ store path v]
      (swap! store assoc-in path v))}})

(def nexus-with-batched-save
  {:effects
   {:effects/save
    ^:nexus/batch
    (fn [_ store path-vs]
      (swap! store
             (fn [state]
               (reduce (fn [s [p v]] (assoc-in s p v)) state path-vs))))}})

(deftest execute-test
  (testing "Fails when there is no implementation"
    (is (= (-> (nexus/execute {} {:system (atom {})} [[:effects/save [:number] 3]])
               datafy-errors)
           {:errors [{:phase :execute-effect
                      :effect-k :effects/save
                      :err {:message "No such effect"
                            :data {:available-effects nil}}}]})))

  (testing "Returns result of executing effect"
    (is (= (-> {:effects
                {:effects/save
                 (fn [_ store path v]
                   (swap! store assoc-in path v))}}
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [[:effects/save [:number] 3]]))
           {:results
            [{:effect [:effects/save [:number] 3]
              :res {:existing "Data"
                    :number 3}}]})))

  (testing "Processes effects one by one"
    (is (= (-> nexus-with-save
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [[:effects/save [:number] 3]
                 [:effects/save [:name] "Nexus"]]))
           {:results
            [{:effect [:effects/save [:number] 3]
              :res {:existing "Data"
                    :number 3}}
             {:effect [:effects/save [:name] "Nexus"]
              :res {:existing "Data"
                    :number 3
                    :name "Nexus"}}]})))

  (testing "Optionally batch processes effect"
    (is (= (-> nexus-with-batched-save
               (nexus/execute
                {:system (atom {:existing "Data"})}
                [[:effects/save [:number] 3]
                 [:effects/save [:name] "Nexus"]]))
           {:results
            [{:effects [[:effects/save [:number] 3]
                        [:effects/save [:name] "Nexus"]]
              :res {:existing "Data"
                    :number 3
                    :name "Nexus"}}]})))

  (testing "Calls interceptors in order"
    (is (= (let [log (atom [])]
             (-> nexus-with-save
                 (assoc :interceptors
                        [(log-interceptor log 1)
                         (log-interceptor log 2)
                         (log-interceptor log 3)])
                 (nexus/execute
                  {:system (atom {:existing "Data"})}
                  [[:effects/save [:number] 9]]))
             @log)
           [[:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:existing "Data", :number 9}]
            [:after-effect 2 :effects/save {:existing "Data", :number 9}]
            [:after-effect 1 :effects/save {:existing "Data", :number 9}]])))

  (testing "Calls interceptors for batch in order"
    (is (= (let [log (atom [])]
             (-> nexus-with-batched-save
                 (assoc :interceptors
                        [(log-interceptor log 1)
                         (log-interceptor log 2)
                         (log-interceptor log 3)])
                 (nexus/execute
                  {:system (atom {:existing "Data"})}
                  [[:effects/save [:number] 9]]))
             @log)
           [[:before-effect 1 :effects/save]
            [:before-effect 2 :effects/save]
            [:before-effect 3 :effects/save]
            [:after-effect 3 :effects/save {:existing "Data", :number 9}]
            [:after-effect 2 :effects/save {:existing "Data", :number 9}]
            [:after-effect 1 :effects/save {:existing "Data", :number 9}]]))))
