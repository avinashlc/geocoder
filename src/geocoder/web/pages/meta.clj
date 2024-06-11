(ns geocoder.web.pages.meta
  (:require [geocoder.web.htmx :refer [base style]]))

(defn <meta>
  "An example base page using htmx, hyperscript, bulma and a local javascript file
   -> Add libraries, scripts, cdns, css ...
   -> Provide fonts, page title, description any ..."
  [opts & body]
  (let [default-opts {:any              "opts"
                      :base/title       "Krishimandir"
                      :base/description "KM flow builder"}
        update-head  (fn [head]
                       (->> (concat
                             [[:link
                               {:rel  "stylesheet"
                                :href "/css/pico.min.css"}]
                              [:link {:rel  "stylesheet"
                                      :href "/css/main.css"}]
                              [:script {:defer "defer"
                                        :src   "/js/htmx.js"}]
                              ;; server sent events
                              [:script {:defer "defer"
                                        :src   "/js/htmx_sse.js"}]
                              ;; UI magic
                              [:script {:defer "defer"
                                        :src   "/js/hyperscript.js"}]
                              [:script {:defer "defer"
                                        :src   "/js/iconify.js"}]
                              [:script {:defer "defer"
                                        :src   "/js/main.js"}]]
                             head)
                            (into [])))
        merged-opts  (-> default-opts
                         (merge opts)
                         (update :base/head update-head))]
    (apply base
           merged-opts
           [:body (-> (style {:margin  "0 auto"
                              :padding "0"})
                      (merge  opts))
            body])))