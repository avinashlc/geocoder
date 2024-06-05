(ns scratch.core
  (:require [clojure.string :as str]
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

  (count rajasthan)
  (- 19000 (count maharashtra))

  (gc/get-location-info config "gujarat")

  util/iso-languages

  (->> maharashtra last)
  (count maharashtra)
  ;; removes binding to r-eval
  ;; (ns-unmap *ns* 'maharashtra)  
  
  (core/fetch->tx!
   system maharashtra
   :end 2)

  (first (shuffle (core/places node :pull? true)))
  
  (name :vi:vi)

  :rcf)

(/ 3000.0 60)

(comment
  ;; Don't use, unless if the system has beem stopped.  
  (core/-main)
  :rcf)

