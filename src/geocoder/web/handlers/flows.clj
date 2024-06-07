(ns geocoder.web.handlers.flows
  (:require [clojure.string :as str]
            [geocoder.web.handlers.core :as h]
            [geocoder.web.helpers :as hlp]
            [geocoder.web.htmx :refer [$$]]
            [geocoder.util :as util]
            [geocoder.web.pages.main :as main]))

(def ^:private !flows-state (atom nil))

(defn basic-intent-handler-fn [context]
  (let [params (-> context :request :form-params)
        intent (some-> params :flow_intent str keyword)]
    (case intent
      :text [:label "Intent text to match"
             [:input {:name        "flow_sub-intent"
                      :type        "text"
                      :placeholder "regex is supported"}]]
      :template [:label "intent template name"
                 [:input {:name "flow_sub-intent" :type "text"}]]
      :other [:label "Other Intent"
              [:input {:name        "flow_sub-intent"
                       :type        "text"
                       :placeholder "Provide any other known intent"}]]
      nil)))

(def basic-intent-handler
  {:name  :fe/flow-basic-form-intent
   :enter (fn [context]
            (reset! h/test-atom context)
            (assoc context :response 
                   
                   (basic-intent-handler-fn context)))})

(def basic-spec
  [:and [:map
         [:flow/id :uuid]
         [:flow/name :keyword]
         [:flow/intent {:optional true} [:vector [:or :string :keyword]]]
         [:flow/template {:optional true} [:string {:min 1}]]
         [:flow/company :uuid]]
   [:fn {:error/message "Either flow/template or flow/intent must be provided"}
    (fn [{:flow/keys [intent template]}] (or intent template))]])

(defn basic-handler-fn [context]
  (try
    (reset! !flows-state nil)
    (let [params (-> context :request :form-params)
          intent (some-> params :flow_intent str keyword)
          sub-intent (some-> params :flow_sub-intent)
          doc (-> params
                  (update-vals (fn [v] (or (parse-uuid v) v)))
                  (update-keys (fn [k] (-> k name (str/split #"_")
                                           (->> (apply keyword)))))
                  (update :flow/name #(some-> % str str/trim
                                              not-empty
                                              str/lower-case
                                              (str/replace " " "-")
                                              keyword)))
          intent (case intent
                   :location {:flow/intent [:user-prompt :location]}
                   :image {:flow/intent [:user-prompt :image]}
                   :template {:flow/template sub-intent}
                   :text {:flow/intent (some->> sub-intent str not-empty (conj [:text]))}
                   :other {:flow/intent (some->> sub-intent str not-empty read-string)}
                   nil)
          state (merge doc intent)]
      (reset! !flows-state state))
    (catch Exception e
      (println e)
      {:err "Unknown Error"})))

(def flow-basic-handler
  {:name  :fe/flow-basic-form
   :enter (fn [context]
            (reset! h/test-atom context)
            (let [data (basic-handler-fn context)]
              (if-let [err (:err data)]
                (h/res context [:div {:id "display-flow-error"
                                      :_ "on click hide me"}
                                [:pre {:style ($$ {:color "salmon"
                                                   :padding "1rem"})}
                                 [:strong (hlp/pprint-str err)]]])
                (h/redirect context "/flow/builder/"))))})

(def flow-builder
  {:name  :fe/flow-builder
   :enter (fn [context]
            (reset! h/test-atom context)
            (->> (or @!flows-state {:err "Unknown Error"})
                 main/flow-draw
                 (h/res context)))})

(def flow-handler
  {:name  :fe/flow
   :enter (fn [context] (h/res context (main/flow-form)))})
