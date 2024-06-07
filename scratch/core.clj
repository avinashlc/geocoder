(ns scratch.core
  (:require [clojure.string :as str]
            [geocoder.api.coords :as gc]
            [geocoder.core :as core]
            [geocoder.state :refer [system]]
            [geocoder.util :as util]
            [throttler.core :as thr]
            [tick.core :as t]))

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
  (defn parse-row [v]
    (-> v
        (update-keys (comp
                      read-string
                      #(str/replace % "-" "/")
                      str))
        (update-vals (fn [v] (if (number? v)
                               (str (int v))
                               v)))))

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
