(ns geocoder.server.core
  (:require [io.pedestal.http :as http]
            [io.pedestal.route :as route]))

(defonce !server (atom nil))

(defn respond-hello [& _req]
  {:status 200 :body "Hello, world!"})

(def routes
  (route/expand-routes
   #{["/" :get respond-hello :route-name :greet]}))

(def srv {::http/routes routes
          ::http/type :jetty
          ::http/port 8890})

(defn create-server [srv]
  (http/create-server srv))   

(defn start []
  (http/start (create-server srv)))

(defn dev-start! []
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
  (dev-start!))