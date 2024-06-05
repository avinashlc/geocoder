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

(defn places [node & {p? :pull?}]
  (->> (xt/q
        (xt/db node)
        {:find [(if p? '(pull ?e [*]) '?e)]
         :where ['[?e :entity/type :place]]})
       (map first)))

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
             (util/parse-spreadsheet "pending_geocodes.xlsx"
                                     "Maharashtra"
                                     :row/fn parse-row)))

  (defonce rajasthan
    (->> (util/parse-spreadsheet
          "pending_geocodes.xlsx" "Rajasthan"
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

  (core/build->tx!
   config node maharashtra
   :start 0
   :end 1000
   :interval 5
   :trial? true)



  (core/csv->tx! config node
                 (shuffle maharashtra)
                 :start 150
                 :end 170
                 :interval 5
                 :trial? true)

  (count (places node))
  :rcf)

(/ 3000.0 60)

(comment
  ;; Don't use, unless if the system has beem stopped.  
  (core/-main)
  :rcf)

