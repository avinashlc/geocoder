(ns geocoder.web.server
  (:require [clojure.pprint :as pprint]
            [geocoder.web.config :refer [config]]
            [geocoder.web.handlers.core :as h]
            [geocoder.web.handlers.main :as main]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :refer [body-params]]
            [io.pedestal.http.ring-middlewares :refer [multipart-params]]
            [io.pedestal.http.sse :as sse]
            [io.pedestal.http.route :as route]))

(defonce !server (atom nil))

(def echo
  {:name  :fe/echo
   :enter (fn [ctx] (h/res ctx ctx))})

(def routes
  (route/expand-routes
   [[["/" ^:interceptors [(body-params)]
      {:get `main/main-handler}]

     ["/stat" {:get `main/stats-view}]

     ["/form" ^:interceptors [(multipart-params)]
      {:post `main/form-handler
       :get  `main/form-view}]

     ["/reset" {:get `main/reset-handler}]

     ["/transaction"
      ["/start" {:get `main/start-transaction}]
      ["/cancel" {:get `main/cancel-transaction}]
      ;;  SSE
      ["/sse" {:get [:sse/geolocation-transact
                     (sse/start-event-stream main/stats-stream)]}]]

     ["/db/info"
      {:get `main/db-info-handler}]

     ["/echo"
      ^:interceptors [(body-params)]
      {:post `echo}]]]))

(def srv
  {::http/routes            routes
   ::http/type              :jetty
   ::http/host              "0.0.0.0"
   ::http/port              8890
   ::http/resource-path     "public"
   ::http/container-options {:h2c? true
                             :h2?  false
                             :ssl? false}
   ::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}})

(defn create-server [srv]
  (http/create-server srv))

(defn start! [sys]
  (alter-var-root #'config (constantly (:config sys)))
  (println (str "please open your browser and visit:  http://localhost:" (::http/port srv) "/") "\n")
  (http/start (create-server srv)))

(defn dev-start! [sys]
  (alter-var-root #'config (constantly (:config sys)))
  (println "Starting Dev Server")
  (pprint/pprint config)
  (let [srv    (assoc srv
                      ::http/join? false
                      ::http/allowed-origins (constantly true))
        server (http/create-server
                (assoc srv
                       ::http/join? false))]
    (reset! !server (http/start server))))

(defn dev-stop! []
  (http/stop @!server))

(defn dev-restart! []
  (dev-stop!)
  (dev-start! {:config config}))
