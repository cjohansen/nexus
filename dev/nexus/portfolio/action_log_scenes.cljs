(ns nexus.portfolio.action-log-scenes
  (:require [dataspex.data :as data]
            [dataspex.hiccup :as hiccup]
            [dataspex.icons]
            [dataspex.ui]
            [nexus.inspector :as inspector]
            [portfolio.replicant :refer-macros [defscene]]))

(def log
  [{:dispatched-at #inst "2025-06-11T08:09:12"
    :dispatch-elapsed 23
    :actions (inspector/->actions [[:task/add "task-id" {:task/title "Do chores"}]])
    :dispatch-data {:number 42}
    :effects (inspector/->actions
              [[:db/transact
                [{:task/id "task-id"
                  :task/title "Do chores"}]]
               [:form/clear [:task-form "task-id"]]])
    :results [{:effect (inspector/->action
                        [:db/transact
                         [{:task/id "task-id"
                           :task/title "Do chores"}]])
               :res [1 :form/id "form" 536870913 true]}]
    :expansions
    [{:action (inspector/->action [:task/add "task-id" {:task/title "Do chores"}])
      :expansion (inspector/->actions
                  [[:db/transact
                    [{:task/id "task-id"
                      :task/title "Do chores"}]]
                   [:form/clear [:task-form "task-id"]]])}]
    :dom-event (let [event (js/MouseEvent. "click" #js {:bubbles true
                                                        :cancelable true
                                                        :view js/window})]
                 (js/document.body.dispatchEvent event)
                 event)}
   {:dispatched-at #inst "2025-06-11T08:07:23"
    :dispatch-elapsed 17
    :actions (inspector/->actions
              [[:task/add "task-id" {:task/title "Do chores"}]
               [:form/clear [:task-form "task-id"]]])}
   {:dispatched-at #inst "2025-06-11T08:06:58"
    :dispatch-elapsed 31
    :actions (inspector/->actions
              [[:db/transact
                [{:task/id "task-id"
                  :task/title "Do chores"}]]])}])

(defn render-dictionary [x & [opt]]
  (-> (data/nav-in x (:dataspex/path opt []))
      (data/inspect opt)
      (hiccup/render-dictionary opt)))

(defscene action-list
  (render-dictionary (inspector/->LogInspector log)))

(defscene action-details
  (-> (inspector/->LogInspector log)
      (render-dictionary {:dataspex/path [(inspector/->ActionKey 0 #inst "2025-06-11T08:09:12")]})))

(defscene less-action-details
  (-> (inspector/->LogInspector log)
      (render-dictionary {:dataspex/path [(inspector/->ActionKey 1 #inst "2025-06-11T08:07:23")]})))
