(ns nexus.hiccup-helper
  (:require [dataspex.panel :as panel]
            [dataspex.ui :as ui]
            [lookup.core :as lookup]
            [nexus.inspector :as inspector]))

(defn render-panel [log & [path]]
  (panel/render-inspector
   {"Actions"
    {:dataspex/path (or path [])
     :dataspex/activity :dataspex.activity/browse
     :dataspex/inspectee "Actions"
     :dataspex/auditable? false
     :val (inspector/->LogInspector (assoc log :now (inspector/now)))}}))

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
