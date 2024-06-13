(ns scratch.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.ative.docjure.spreadsheet :as st]
            [geocoder.api.coords :as gc]
            [geocoder.core :as core]
            [geocoder.state :refer [system]]
            [geocoder.util :as util]
            [throttler.core :as thr]
            [tick.core :as t]
            [xtdb.api :as xt]))

(def root-config (:config system))
(def config (:components/place root-config))
(def node (:db/geocodes system))

(comment  
  ;; check, rate limit, if does only one print per second
  (let [+# (thr/throttle-fn + 1 :second)]
    (doseq [a (range 10)]
      (->> (+# a a)
           (println "here " (t/time) "\n" (str (java.util.Date.)) "\n"))))
  :rcf)

(comment
  root-config

  (xt/submit-tx node [[::xt/delete :pop]])


  (-> (io/file ".stuff/pending_geocodes.xlsx")
      str
      st/load-workbook-from-file
      st/sheet-seq)

  (defn parse-row [v]
    (-> v
        (update-keys (comp
                      read-string
                      #(str/replace % "-" "/")
                      str))
        (update-vals (fn [v] (if (number? v)
                               (str (int v))
                               v)))))

  (try
    (map? (:location (gc/geometry! {:google-api "POP"} "India")))
    (catch Exception e
      (println e)
      false))

  {:bounds        {:northeast {:lat 35.6733149
                               :lng 97.39535869999999}
                   :southwest {:lat 6.4626999
                               :lng 68.1097}}
   :location      {:lat 20.593684
                   :lng 78.96288}
   :location_type "APPROXIMATE"
   :viewport      {:northeast {:lat 35.6733149
                               :lng 97.39535869999999}
                   :southwest {:lat 6.4626999
                               :lng 68.1097}}}

  {:lgd           "https://storage.googleapis.com/lgd_data_archive/10Jan2024.zip"
   :resource-path "resources/places/"
   :villages      "villages_by_blocks.csv"
   :pincodes      "pincode_villages.csv"
   :geocode-url   "https://maps.googleapis.com/maps/api/geocode/json"
   :google-api    "AIzaSyAbUAqS62q9L93s0E8KOGKsvCfjJcHe6HE"}

  (defonce maharashtra
    (sort-by :village/code
             (util/parse-spreadsheet ".stuff/pending_geocodes.xlsx"
                                     "Maharashtra"
                                     :row/fn parse-row)))

  (defonce rajasthan
    (->> (util/parse-spreadsheet
          ".stuff/pending_geocodes.xlsx" "Rajasthan"
          :row/fn parse-row)
         (sort-by :village/code)
         (take 10000)))

  (defonce UP
    (->> (util/parse-spreadsheet
          ".stuff/pending_geocodes.xlsx" "uttar pradesh"
          :row/fn parse-row)
         (sort-by :village/code)
         (take 13000)))

  (count rajasthan)
  (- 19000 (count maharashtra))

  (try
    (gc/get-location-info config "gujarat")
    (catch Exception e e))

  util/iso-languages

  (->> maharashtra last)
  (count maharashtra)
  ;; removes binding to r-eval
  ;; (ns-unmap *ns* 'maharashtra)  

  (core/places node :count? true)

  (core/fetch->tx! system UP :end 10)

  (count (core/places node :pull? true))

  (first (shuffle (core/places node :pull? true)))

  (gc/coordiantes->location
   config
   {:longitude 73.7874983
    :latitude  29.6292936}
   :get-all? true)

  :rcf)


(comment
  ;; Don't use, unless if the system has beem stopped.  
  (core/-main)
  :rcf)
