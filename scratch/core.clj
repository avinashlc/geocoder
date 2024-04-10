(ns scratch.core
  (:require [geocoder.core :as core]
            [geocoder.state :refer [system]]
            [throttler.core :as thr]
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
  (let [+# (thr/throttle-fn + 5 :second)]
    (doseq [a (range 60)]
      (->> (+# a a)
           (println "hereh" (str (java.util.Date.))))))
  :rcf)

(comment
  (defonce maharashtra (core/by-state config "maharashtra")) ;; gets data from the csv
  (->> maharashtra shuffle (take 5))
  (count maharashtra)
  ;; removes binding to r-eval
  ;; (ns-unmap *ns* 'maharashtra)    

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

