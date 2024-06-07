(ns geocoder.web.ui
  (:require [geocoder.web.htmx :refer [$$]]))

(defn snippet
  [params & body]
  [:pre (merge params
               {:style ($$ {:color "#fcfcfc"
                            :padding "1rem"})})
   [:strong body]])

(defn option
  [& {l :label
      v :value
      :as opts}]
  [:option (merge opts {:value (str v)})
   (or (str l) (str v))])

(defn select
  [& {action :action
      sname  :name
      rfn    :render
      col    :contents
      df     :default}]
  [:select (merge action {:name sname})
   (or df (option :label "select" :value "nil"))
   (map (or rfn option) col)])