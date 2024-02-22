(ns scratch.core
  (:require [geocoder.core :as core]
            [geocoder.state :refer [system]]
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
  (defonce maharashtra (core/by-state config "maharashtra"))
  (->> maharashtra shuffle (take 5))
  (count maharashtra)
  ;; removes binding to r-eval
  (ns-unmap *ns* 'maharashtra)    

  (core/csv->tx! config node
                 (shuffle maharashtra)
                 :start 150
                 :end 170
                 :interval 5
                 :trial? true)

  (count (places node))
  :rcf)

(comment
  ;; Don't use, unless if the system has beem stopped.  
  (core/-main)
  :rcf)

