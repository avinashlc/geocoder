(ns geocoder.web.pages.main
  (:require [geocoder.web.helpers :as hlp]
            [geocoder.web.htmx :refer [$$]]
            [geocoder.web.ui :as ui]
            [geocoder.web.pages.meta :refer [<meta>]]))

(defn flow-form [& _args]
  (<meta> {:data-theme "dark"
           :style ($$ {:margin "0 auto"
                       :padding "0"})}
    [:main.container
     [:h1 [:mark "GEOCODER!!"]]
     [:div {:id "display-flow-error"
            :_ "on click hide me"}]
     [:form {:hx-post "/flow/form/basic/"
             :hx-target "#display-flow-error"
             :hx-swap "outerHTML"}
      [:label "Flow ID (Only change the ID if it's needed)"
       [:input {:name "flow_id" :type "text" :value (str (random-uuid))}]]
      [:label "Flow Name"
       [:input {:name "flow_name" :type "text"}]]
      [:label "Flow Intent"
       (ui/select
        :action {:hx-post   "/flow/form/basic/intent"
                 :hx-target "#flow_intent-sub"}
        :name "flow_intent"
        :contents ["text" "location" "image" "template" "other"]
        :render #(ui/option :label %
                         :value %))]
      [:div {:id "flow_intent-sub"}]
      [:label "Flow Company"
       (ui/select
        :name "flow_company"
        :contents [{:xt/id "as" :company/shortname "asas"}]
        :render #(ui/option :label (:company/shortname %)
                         :value (:xt/id %)))]
      [:button {:type "submit"}
       "Open builder"]]]))

(defn steps-content [{id :element/id}]
  [:div {:id id}
   [:h4 "content"]])

(defn- toggle [child]
  (let [tgl  (case child
               :one "two"
               :two "one")
        text (fn [remove add]
               (hlp/space "on click"
                          (str "hide #grid_child_" (name child))
                          "then"
                          (str "remove " remove " from #grid_parent")
                          "then"
                          "add " add " to #grid_parent"
                          "then"
                          (str "show #grid_child_" tgl)))]
    (case child
      :one (text ".grid_parent_single_child"
                 ".grid_parent")
      :two (text
            ".grid_parent"
            ".grid_parent_single_child"))))

(defn steps-list-toggler
  [{id :element/id}]
  [:div {:id    id
         :style ($$ {:display "none"})
         :_     (toggle :one)}
   [:p {:style ($$ {:text-align "center"
                    :display    "inline-block"
                    :vertical-align "sub"})}
    [:iconify-icon {:icon "line-md:watch-loop"}]]])

(defn steps-list [{id :element/id}]
  [:div {:id id
         :style ($$ {:background-color "var(--code-background-color)"})}
   [:h4 {:style ($$ {:text-align     "center"
                     :height "0"})} "steps"
    [:p {:style ($$ {:margin-left "5%"
                     :display    "inline-block"
                     :vertical-align "sub"})
         :_     (toggle :two)}
     [:iconify-icon {:icon "line-md:watch-off-twotone-loop"}]]]
   [:hr]])

(defn flow-draw [params]
  (<meta> {:base/head  [[:script {:defer "defer"
                                  :src   "/js/sortable.js"}]]
           :data-theme "dark"
           :style      ($$ {:margin  "0 auto"
                            :padding "0"})}
    [:main.container
     [:h1 [:mark "FLOW!"] " Builder"]
     [:div [:pre {:style ($$ {:padding "1rem"
                              :color   (if (:ok params) "cream" "salmon")})}
            [:strong (hlp/pprint-str (or (:ok params) (:err params)))]]
      (when (:err params)
        [:a {:href "/flow/form/"}
         [:small "Click here to fill up"
          [:strong " basic flow information, "]
          "if you haven't done yet"]])
      [:div.grid_parent.container-fluid
       {:id "grid_parent" :style ($$ {:margin-top "1rem"})}
       (steps-list-toggler {:element/id "grid_child_one"})
       (steps-list {:element/id "grid_child_two"})
       (steps-content {:element/id "grid_child_two"})]]]))