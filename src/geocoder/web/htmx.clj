(ns geocoder.web.htmx)

(defn style
  "Receives a map of style attributes as key-val and returns a map with a single key called :style, 
   holding all the style attributes in a string as required by html inline styles.   
   e.g: (style {:margin '45px' :text-align 'center}) -> {:style 'margin:45px; text-align:center;'}     
   "
  [params & {:keys [str?]}]
  (let [fmt-str #(let [[kwd val] %]
                   (str (name kwd) ":" val "; "))
        style-str (->> params (map fmt-str) (apply str) .trim)]
    (if str?
      style-str
      {:style style-str})))

(def $
  "```clojure
   ($ {:margin '45px' :text-align 'center}) -> {:style \"margin:45px; text-align:center;\"}     
   ```"
  style)

(defn $$
  "```clojure
   ($ {:margin '45px' :text-align 'center}) -> \"margin:45px; text-align:center;\"
   ```"
  [p] (style p :str? true))

(defn g-fonts
  [families]
  [:link {:href (apply str "https://fonts.googleapis.com/css2?display=swap"
                       (for [f families]
                         (str "&family=" f)))
          :rel "stylesheet"}])

(defn base
  [{:base/keys [title
                description
                lang
                image
                icon
                url
                canonical
                font-families
                head]
    :or        {title         "Geocoder"
                lang          "en-US"
                font-families ["Material+Icons"]
                icon          "/img/star.gif"
                description   "Geocoder"
                image         "https://clojure.org/images/clojure-logo-120b.png"}}
   & contents]
  [:html
   (merge
    {:lang lang}
    (style {:min-height "100%"
            :height     "auto"}))
   [:head
    [:title title]
    [:meta {:name    "description"
            :content description}]
    [:meta {:content  title
            :property "og:title"}]
    [:meta {:content  description
            :property "og:description"}]
    (when image
      [:meta {:content  image
              :property "og:image"}])
    [:meta {:content "summary"
            :name    "twitter:card"}]
    (when-some [url (or url canonical)]
      [:meta {:content  url
              :property "og:url"}])
    (when-some [url (or canonical url)]
      [:link {:ref  "canonical"
              :href url}])
    [:meta {:name    "viewport"
            :content "width=device-width, initial-scale=1"}]
    (when icon
      [:link {:rel   "icon"
              :type  "image/png"
              :sizes "16x16"
              :href  icon}])
    [:meta {:charset "utf-8"}]
    (when (not-empty font-families)
      (list
       [:link {:href "https://fonts.googleapis.com"
               :rel  "preconnect"}]
       [:link {:crossorigin "crossorigin",
               :href        "https://fonts.gstatic.com",
               :rel         "preconnect"}]
       (g-fonts font-families)))
    (apply list head)]
   (into [] contents)])

(defn example-page
  "An example base page using htmx, hyperscript, bulma and a local javascript file
   -> Add libraries, scripts, cdns, css ...
   -> Provide fonts, page title, description any ..."
  [opts & body]
  (let [default-opts {:any "opts"}
        update-head  (fn [head]
                       (concat
                        [#_[:link {:rel  "stylesheet"
                                   :href "/css/main.css"}]
                         [:link
                          {:rel  "stylesheet"
                           :href "/css/pico.min.css"}]

                         #_[:link
                            {:rel  "stylesheet"
                             :href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma-rtl.min.css"}]

                         [:script {:defer "defer"
                                   :src   "/js/htmx.js"}]
                         [:script {:defer "defer"
                                   :src   "/js/hyperscript.js"}]
                         [:script {:defer "defer"
                                   :src   "/js/main.js"}]]
                        head))
        merged-opts  (-> default-opts
                         (merge opts)
                         (update :base/head update-head))]
    (apply base
           merged-opts
           [:body (style (merge {:margin "0 auto" :padding "0"}))
            body])))

(comment
  (-> {:base/title "OPS sales Order"
       :base/description "Ops Sales order applicatoin"}
      (example-page
       [:h1 "Hello"])))