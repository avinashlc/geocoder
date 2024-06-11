(ns geocoder.web.pages.main
  (:require [clojure.string :as str]
            [geocoder.util :as util]
            [geocoder.web.htmx :refer [$$]]
            [geocoder.web.pages.meta :refer [<meta>]]))

(defn <>
  [& body]
  (<meta> {:data-theme "dark"
           :style      ($$ {:margin  "0 auto"
                            :padding "0"})}
          [:main.container
           [:header
            [:h1 [:i "GEOCODER!!"]]
            [:h1 [:button.outline {:hx-get "/reset"
                                   :style  ($$ {:color "salmon"})}
                  "reset form!"]]]
           body]))

(defn <ip> [fdt k title & {:as args}]
  (let [reqd [:span {:style ($$ {:color "yellowgreen"})} [:strong " < REQ >"]]
        cmp [:label title (when (:required? args) reqd)
             [:input (-> (dissoc args :required?)
                         (merge {:name  (name k)
                                 :value (k fdt)})
                         (update :type #(or % "text"))
                         util/remove-nil-vals)]]]
    (vec (remove nil? cmp))))

(defn form [& {!state :form/state}]
  (let [<ip> (partial <ip> @!state)]
    (<>
     [:section
      [:div {:id "display-form-info"
             :_  "on click set #form-submit @aria-busy to 'false' then hide me"}]
      [:form {:hx-encoding "multipart/form-data"
              :hx-post     "/form"
              :hx-target   "#display-form-info"
              :hx-swap     "outerHTML"}
       (<ip> :google-api "Google Geolocation API"
             :required? true
             :type "password")
       (<ip> :state "State Name" :required? true)
       (<ip> :start "Position Start"
             :type        "number"
             :placeholder "Get items from this position of the parsed data")
       (<ip> :end "Position End"
             :type        "number"
             :placeholder "Stop Gettting items at this position of the parsed data")
       (<ip> :interval "Interval"
             :type        "number"
             :placeholder "Splits the parsed data into mulitple sections based on the given interval")
       (<ip> :spreadsheet "Spreadsheet"
             :required?  true
             :type        "file"
             :accept      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
       [:button {:id "form-submit"
                 :type "submit"
                 :_ "on click set @aria-busy to 'true'"} "Fetch and Transact!"]]])))

(defn basic-table [fdt]
  [:table
   [:tr
    [:th {:scope "row"} [:strong  "Google API token"]]
    [:td {:scope "row"}
     [:i
      (let [[v r] (split-at 5 (:google-api fdt))
            sx    (partial apply str)]
        (str (sx v)  (sx (repeatedly (count r) (constantly "*")))))]]]

   (when (:start fdt)
     [:tr
      [:th {:scope "row"} [:strong "Start"]]
      [:td {:scope "row"} [:i (:start fdt)]]])

   (when (:end fdt)
     [:tr
      [:th {:scope "row"} [:strong "End"]]
      [:td {:scope "row"} [:i (:end fdt)]]])

   (when (:interval fdt)
     [:tr
      [:th {:scope "row"} [:strong "Interval"]]
      [:td {:scope "row"} [:i (:interval fdt)]]])

   [:tr
    [:th {:scope "row"} [:strong "Spreadsheet"]]
    [:td {:scope "row"} [:i (:spreadsheet fdt)]]]])

(defn stat
  [& {!state :form/state}]
  (<>
   [:article
    (basic-table @!state)
    [:div {:id "stat-sse"}
     [:section
      [:button.outline
       {:hx-get   "/transaction/cancel"
        :id        "transaction-handler"
        :style     ($$ {:color "salmon"})
        :hx-target "#stat-sse"
        :hx-swap "innerHTML"}
       "cancel transaction :c"]]
     [:div {:id          "sse-container"
            :hx-ext      "sse"
            :sse-connect "/transaction/sse"}
      [:section {:sse-swap  "message"}]
      [:div {:id        "sse-exit"
             :sse-swap  "transaction-complete"
             :hx-target "#stat-sse"}]]]]))