(ns geocoder.web.pages.main
  (:require [geocoder.util :as util]
            [geocoder.web.htmx :refer [$$]]
            [geocoder.web.pages.meta :refer [<meta>]]))

(defn info [data]
  (let [row   (fn [[state count]]
                [:tr
                 [:th {:scope "row"} [:strong (str state)]]
                 [:td {:scope "row"} [:i (str count)]]])
        rows  (mapv row (:states data))
        total [:tr
               [:th {:scope "row"} ""]
               [:td {:scope "row"} [:strong (str (:total data))]]]
        table (vec (concat [:table
                            [:tr {:data-theme "light"}
                             [:th {:scope "row"} [:strong "State"]]
                             [:td {:scope "row"} [:strong "In database"]]]]
                           (conj rows total)))
        hdr   [:div.grid
               [:h3 "Information"]
               [:span {:id        "db-info-loading"
                       :aria-busy "true"
                       :style     ($$ {:display "none"})}
                "Querying the database ..."]
               [:span.streamline--database-refresh-solid
                {:_         "on click show #db-info-loading"
                 :hx-get    "/db/info"
                 :hx-target "#db-info"
                 :hx-swap   "outerHTML"
                 :style     ($$ {:margin-top  "2.5%"
                                 :margin-left "90%"
                                 :color       "salmon"
                                 :height      "50%"})}]]]
    [:section {:id    "db-info"
               :style ($$ {:margin-top "10%"
                           :margin-bottom "20%"})}
     hdr
     table]))

(defn <>
  [{aps :app/state} & body]
  (<meta> {:data-theme "dark"
           :style      ($$ {:margin  "0 auto"
                            :padding "0"})}
          [:main.container
           [:header
            [:h1 [:i "GEOCODER!!"]]
            [:h1 [:button.outline {:hx-get "/reset"
                                   :style  ($$ {:color "salmon"})}
                  "reset form!"]]]
           body
           (some-> aps :db/info not-empty info)]))

(defn <ip> [fdt k title & {:as args}]
  (let [reqd [:span {:style ($$ {:color "yellowgreen"})} [:strong " < REQ >"]]
        cmp  [:label title (when (:required? args) reqd)
              [:input (-> (dissoc args :required?)
                          (merge {:name  (name k)
                                  :value (k fdt)})
                          (update :type #(or % "text"))
                          util/remove-nil-vals)]]]
    (vec (remove nil? cmp))))

(defn form [& {!state :form/state
               :as    args}]
  (let [<ip> (partial <ip> @!state)]
    (<> args
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
          [:button {:id   "form-submit"
                    :type "submit"
                    :_    "on click set @aria-busy to 'true'"} "Fetch and Transact!"]]])))

(defn basic-table [fdt]
  [:table
   [:tr
    [:th {:scope "row"} [:strong  "Google API token"]]
    [:td {:scope "row"}
     [:i
      (let [k     (:google-api fdt)
            [v r] (split-at (- (count k) 5) k)
            sx    (partial apply str)]
        (str (sx (repeatedly (count v) (constantly "*")))  (sx r)))]]]

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
  [& {!state :form/state
      :as    args}]
  (<> args
      [:article
       (basic-table @!state)
       [:div {:id "stat-sse"}
        [:section
         [:button.outline
          {:hx-get    "/transaction/cancel"
           :id        "transaction-handler"
           :style     ($$ {:color "salmon"})
           :hx-target "#stat-sse"
           :hx-swap   "innerHTML"}
          "cancel transaction :c"]]
        [:div {:id          "sse-container"
               :hx-ext      "sse"
               :sse-connect "/transaction/sse"}
         [:section {:sse-swap "message"}]
         [:div {:id        "sse-exit"
                :sse-swap  "transaction-complete"
                :hx-target "#stat-sse"}]]]]))