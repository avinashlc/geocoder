(ns geocoder.web.handlers.core 
  (:require [geocoder.web.response :as response]
            [hiccup.page :refer [html5]]))

(def test-atom (atom nil))

(defn auth-filter [context query]
  (let [auth-filter (-> context :request :auth-filter)
        single? (map? query)
        more-but-nested? (vector? (first query))]
    (cond
      single? (-> [query] auth-filter first)
      more-but-nested? (->> query (map first) auth-filter)
      :else (auth-filter query))))

(defn form-file [context]
  (let [request (:request context)
        path-params (:path-params request)
        error-id (str "ui-" (:form path-params) "-" (:field path-params))
        field-name (str (:form path-params) "/" (:field path-params))
        _value (get (:params request) field-name)
        value (if (vector? _value) _value [_value])]
    {:value value
     :error-id error-id}))

#_(defn view-attachment [context]
    (let [path-params (-> context :request :path-params)
          atch (-> path-params
                   :attachment-id
                   parse-uuid
                   attachment/get-attachment)
          src (:attachment/src atch)
          info (util/file-info src)
          content-type (case (:ext info)
                         "jpg" "image/jpeg"
                         "jpeg" "image/jpeg"
                         "png" "image/png"
                         (str "application/" (:ext info)))]
      {:src (io/file src)
       :content-type content-type}))

(defn res [context body & {:keys [headers status]}]
  (let [response (-> body
                     html5
                     (response/ok "Content-Type" "text/html"))]
    (-> context
        (assoc :response response)
        (update-in [:response :headers] merge headers)
        (update-in [:response :status] #(or status %)))))

#_(defn display-error [context ex]
    (log/info :msg "Page Not found"
              :error ex)
    (res context (not-found)))

#_(defn page-not-found [context ex]
    (log/info :msg "Page Not found"
              :error ex)
    (res context (not-found)))

(defn redirect [context url]
  (res context ""
       :headers {"HX-Redirect" url}))

