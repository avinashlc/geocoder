(ns geocoder.web.handlers.main
  (:require [clojure.core.async :as a]
            [clojure.core.async.impl.protocols :as chan]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [geocoder.api.coords :as gc]
            [geocoder.transact :as tx]
            [geocoder.util :as util]
            [geocoder.web.config :refer [config]]
            [geocoder.web.handlers.core :as h]
            [geocoder.web.helpers :as hlp]
            [geocoder.web.htmx :refer [$$]]
            [geocoder.web.pages.main :as main]
            [hiccup.page :refer [html5]]
            [juxt.clip.repl :as clip]
            [malli.core :as m]
            [malli.error :as me]
            [geocoder.scheduler :as task]
            [com.rpl.specter :as s]))

(def ^:private !form-state (atom nil))
(def ^:private !system (atom nil))
(def ^:private task-id :stats/transaction)

(defn db-info []
  (let [sys  (if (:db/geocodes @!system)
               @!system
               (reset! !system (or (not-empty clip/system)
                                   (util/initiator! config))))]
    (tx/info (:db/geocodes sys))))

(comment
  @!form-state
  :rcf)

(def api-spec
  [:and [:string
         {:error/message "Should Not be an empty string"
          :min 1}]
   [:fn {:error/message "Invalid token! Failed to make a request using that token"}
    (fn [v]
      (try
        (map? (some-> (not-empty config)
                      :components/place
                      (select-keys [:geocode-url])
                      (merge {:google-api v})
                      (gc/geometry! "India")
                      :location))
        (catch Exception e
          (println e)
          false)))]])

(def form-spec
  [:map
   [:google-api api-spec]
   [:state [:string {:min 1}]]
   [:start {:optional true} :int]
   [:end {:optional true} :int]
   [:interval {:optional true} :int]
   [:spreadsheet [:string {:min 1}]]])

(defn save-file
  [file-obj path-to-save & {:keys [file-name]}]
  (when file-obj
    (let [[in _file-name] ((juxt :tempfile :filename) file-obj)
          info            (util/file-info _file-name)
          file-path       (str path-to-save "/" (or file-name (:name info)) "." (:ext info))
          _copy           (if (.exists (io/file path-to-save))
                            (io/copy in (io/file file-path))
                            (do (io/make-parents file-path)
                                (io/copy in (io/file file-path))))]
      file-path)))

(defn basic-handler-fn [context]
  (try
    (reset! !form-state nil)
    (let [params (-> context :request :params)
          pl     #(some-> (not-empty %)
                          str
                          parse-long)
          ky     (fn [k] (-> k name (str/split #"_")
                             (->> (apply keyword))))
          doc    (-> params
                     (update-keys ky)
                     (update :start pl)
                     (update :end pl)
                     (update :interval pl)
                     (update :spreadsheet save-file "resources")
                     util/remove-nil-vals)]
      (reset! !form-state doc)
      (if (m/validate form-spec doc)
        {:ok doc}
        {:err (me/humanize (m/explain form-spec doc))}))
    (catch Exception e
      (println e)
      {:err (str "Unexpected Error: " (.getMessage e))})))

(defn -form-handler [context]
  (let [data (basic-handler-fn context)
        txt (fn [err]
              [:div {:id "display-form-info"
                     :_  "on click set #form-submit @aria-busy to 'false' then hide me"}
               [:pre {:style ($$ {:color   "salmon"
                                  :padding "1rem"})}
                [:strong (hlp/pprint-str err)]]])]
    (if-let [err (:err data)]
      (h/res context (txt err))
      (if-not (empty? (reset! !system (or (not-empty clip/system)
                                          (util/initiator! config))))
        (let [opts (set/rename-keys
                    @!form-state
                    {:spreadsheet :path})]
          (->> (not-empty (tx/parse! @!system opts))
               (swap! !form-state assoc :data)
               util/remove-nil-vals)
          (h/redirect context "/stat"))
        (h/res context (txt {:SYSTEM "Error Initializing the system. Try again!"}))))))

(def form-handler
  {:name  :web/form
   :enter -form-handler})

(def reset-handler
  {:name  :ui/stat
   :enter (fn [context]
            (reset! !form-state nil)
            (h/redirect context "/form"))})

(def cancel-transaction
  {:name  :stat/cancel-transaction
   :enter (fn [context]
            (task/cancel-task! task-id)
            (h/res context
                   [:section
                    [:button.outline
                     {:hx-get   "/transaction/start"
                      :id        "transaction-handler"
                      :hx-target "#stat-sse"
                      :hx-swap "innerHTML"}
                     "start transaction c:"]]))})

(def start-transaction
  {:name  :stat/cancel-transaction
   :enter (fn [context]
            (h/redirect context "/stat"))})

(def stats-view
  {:name  :ui/stat
   :enter (fn [context]
            (h/res context
                   (main/stat :form/state !form-state
                              :form/spec  form-spec
                              :app/state  {:db/info (db-info)})))})

(def form-view
  {:name  :ui/form
   :enter (fn [context]
            (h/res context
                   (main/form :form/state !form-state
                              :form/spec  form-spec
                              :app/state  {:db/info (db-info)})))})

(def main-handler
  {:name  :web/main
   :enter (fn [context]
            (let [goto (if (m/validate form-spec @!form-state)
                         "/stat"
                         "/form")]
              (assoc context :response
                     {:status  302
                      :body    ""
                      :headers {"Location" goto}})))})

(def db-info-handler
  {:name  :stat/cancel-transaction
   :enter (fn [context]
            (h/res context (main/info (db-info))))})

(def ^:private !event-channel (atom nil))

(defn stats-stream [evt-chan _context]
  (when @!event-channel (a/close! @!event-channel))
  (reset! !event-channel evt-chan)
  (let [>!! (fn blocking>>
              ([data]
               (blocking>> :message data))
              ([evt data]
               (a/>!! @!event-channel
                      {:name (name evt)
                       :data (html5 data)})))
        txt (fn [msg & {color :color}]
              [:pre {:style ($$ {:color   (or color "salmon")
                                 :padding "1rem"})}
               [:strong msg]
               " Please check your form details before trying again"])]
    (>!! [:progress])
    (when-not (boolean (task-id @task/!tasks))
      (task/background-task!
       task-id
       (fn [_exec-time]
         (cond
           (empty? @!system) (>!! @!event-channel (txt "System initialization Failed!"))
           :else (try
                   (if (m/validate form-spec @!form-state)
                     (let [base [:table
                                 [:thead
                                  [:tr
                                   [:th {:scope "col"} "Iteration"]
                                   [:th {:scope "col"} "Time"]
                                   [:th {:scope "col"} "Target"]
                                   [:th {:scope "col"} "Transacted"]
                                   [:th {:scope "col"} "In DB"]
                                   [:th {:scope "col"} "Total in DB"]]]]
                           opts (set/rename-keys
                                 @!form-state
                                 {:spreadsheet :path})]
                       (when-not (chan/closed? @!event-channel)
                         (if-let [data (not-empty (:data @!form-state))]
                           (let [pfn  (fn [info]
                                        [:section
                                         (conj base
                                               [:tbody
                                                [:tr
                                                 [:th {:scope "row"} (:iteration info)]
                                                 [:th {:scope "row"} (:time info)]
                                                 [:th {:scope "row"} (:target info)]
                                                 [:th {:scope "row"} (:transacted info)]
                                                 [:th {:scope "row"} (:in-db info)]
                                                 [:th {:scope "row"} (:total-in-db info)]]
                                                (when (and (:transacted info) (:target info))
                                                  [:tr
                                                   [:td {:scope   "row"
                                                         :colspan "6"}
                                                    [:progress {:value (let [at  (if (:interval info)
                                                                                   (* (:index info) (:interval info))
                                                                                   (+ (:in-db info) (:transacted info)))
                                                                             rio (/ at (:target info))
                                                                             prc (* rio 100)]
                                                                         (int prc))
                                                                :max   100}]]])
                                                (when (:completed? true)
                                                  [:tr
                                                   [:td {:scope   "row"
                                                         :colspan "6"}
                                                    [:i [:strong "Transaction Completed"]]]])])])
                                 opts (assoc opts :post-process (comp >!! pfn))
                                 tx!  (some-> @!system
                                              (assoc-in [:config :components/place :google-api]
                                                        (:google-api opts))
                                              (tx/fetch->tx! data opts))]
                             (when (= :ok tx!)
                               (>!! :transaction-complete
                                    (pfn {:completed?  true
                                          :total-in-db (tx/places (:db/geocodes @!system) :count? true)}))
                               (swap! task/!tasks dissoc task-id)))
                           (>!!  (txt "No data found to fetch!")))))
                     (when-not (chan/closed? @!event-channel)
                       (>!!  (txt "Invalid Form State!"))))
                   (catch Exception e
                     (println (.getMessage e))
                     (pp/pprint e)
                     (>!!  :transact-complete (txt (.getMessage e))))))
         (a/close! @!event-channel))))))