(ns nexus.portfolio.scenes
  (:require [portfolio.ui :as portfolio]
            [replicant.dom :as r]))

(r/set-dispatch!
 (fn [_ event-data]
   (prn event-data)))

(def light-theme
  {:background/background-color "#fff"
   :background/document-class "light"})

(def dark-theme
  {:background/background-color "#18181a"
   :background/document-class "dark"})

(portfolio/start!
 {:config
  {:css-paths ["/dataspex/inspector.css"]
   :background/options
   [{:id :default
     :title "Light"
     :value light-theme}
    {:id :replicant
     :title "Dark"
     :value dark-theme}]

   :canvas/layout {:kind :rows
                   :xs [light-theme
                        dark-theme]}}})
