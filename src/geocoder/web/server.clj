(ns geocoder.web.server
  (:require [geocoder.web.handlers.core :as h]
            [geocoder.web.handlers.flows :as main]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :refer [body-params]]))

(defonce !server (atom nil))

(defonce !system (atom nil))

(def echo
  {:name  :fe/echo
   :enter (fn [ctx] (h/res ctx ctx))})

(def routes
  (route/expand-routes
   [[["/" ^:interceptors [(body-params)]
      {:get `main/flow-handler}]

     ["/echo"
      ^:interceptors [(body-params)]
      {:post `echo}]]]))

(def srv
  {::http/routes         routes
   ::http/type           :jetty
   ::http/host           "0.0.0.0"
   ::http/port           8890
   ::http/resource-path  "public"
   ::http/secure-headers {:content-security-policy-settings {:object-src "none"}}})

(defn create-server [srv]
  (http/create-server srv))

(defn start! [sys]
  (reset! !system sys)
  (http/start (create-server srv)))

(defn dev-start! [sys]
  (reset! !system sys)
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
  (dev-start! @!system))