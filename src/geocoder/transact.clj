(ns geocoder.transact
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [geocoder.api.coords :as gc]
            [geocoder.db]
            [geocoder.scheduler]
            [geocoder.state :refer [system]]
            [geocoder.util :as util]
            [tick.core :as t]
            [xtdb.api :as xt]
            [xtdb.query :as query]))

(defmethod query/aggregate 'group->count [_]
  (fn aggregate-group->count
    ([] [])
    ([acc] (reduce (fn [acc' v] (update acc' v #(or (some-> % inc) 1))) {} acc))
    ([acc x] (conj acc x))))

(defn info [node]
  (let [base   '[[?e :entity/type :place]
                 [?e :latitude ?la]
                 [?e :longitude ?lo]]
        total  (ffirst (xt/q (xt/db node)
                             {:find  '[(count ?e)]
                              :where base}))
        states (->> (xt/q
                     (xt/db node)
                     {:find  '[(group->count ?sn)]
                      :in    '[?clean]
                      :where (conj base
                                   '[?e :state/name ?snr]
                                   '[(?clean ?snr) ?sn])}
                     (comp str/lower-case str/trim))
                    ffirst)]
    {:total  (or total 0)
     :states states}))

(defn places
  [node
   & {p?   :pull?
      c?   :count?
      stc  :state/code
      dic  :district/code
      sdic :subdistrict/code
      vic  :village/code
      id   :xt/id}]
  (let [res (xt/q
             (xt/db node)
             {:find  [(cond p? '(pull ?e [*])
                            c? '(count ?e)
                            :else '?e)]
              :where (cond-> ['[?e :entity/type :place]
                              '[?e :latitude ?la]
                              '[?e :longitude ?lo]]
                       stc  (conj ['?e :state/code stc])
                       dic  (conj ['?e :district/code dic])
                       sdic (conj ['?e :subdistrict/code sdic])
                       vic  (conj ['?e :village/code vic])
                       id   (conj ['?e :xt/id id]))})]
    (cond
      c? (or (ffirst res) 0)
      :else (map first res))))

(defn update-code [item]
  (let [m (fn [k]
            (fn [v]
              (if (keyword? v)
                v
                (keyword (str (name k) v)))))]
    (-> item
        (update :state/code       (m :st))
        (update :district/code    (m :di))
        (update :subdistrict/code (m :sdi))
        (update :village/code     (m :vi)))))

(defn inject-grid-geocode-info
  "Returns the item with geolocation data merged to it, by making a http request to google's api
   - here the **item** is simply parsed csv entity"
  [conf node item
   & {:keys [validator] :or {validator (constantly true)}}]
  (let [item      (update-code item)
        not-incl? #(some-> (util/ok-> string? (:state/name item))
                           str/lower-case
                           (str/includes? %)
                           false?)
        in-db     (util/ok->
                   (fn [v]
                     (and (-> v :latitude number?)
                          (-> v :longitude number?)))
                   (some->> (assoc item :pull? true)
                            (places node)
                            first))
        ok?       (and (not-incl? "english")
                       (not-incl? "state name")
                       (validator item))]
    (try
      (if in-db
        {:err (str "Already fetched coords for " (:xt/id in-db))}
        (when ok?
          (let [address (->> ["India"
                              (:state/name item)
                              (:district/name item)
                              (:subdistrict/name item)
                              (:village/name item)]
                             (remove nil?)
                             (str/join ", "))
                grid    (gc/indian-address->grid conf address 5)
                res     (some-> item
                                (assoc :latitude (:latitude grid)
                                       :longitude (:longitude grid)
                                       :proximity-grid/x (-> grid :grid/address :x)
                                       :proximity-grid/y (-> grid :grid/address :y)
                                       :proximity-grid/size (:grid/size grid))
                                (update :entity/type #(or % :place))
                                (update-vals #(if (string? %) (str/lower-case %) %))
                                (update :xt/id #(or % (:village/code item))))]
            (when res {:ok res}))))
      (catch Exception e
        (println "Someting Went wrong " (.getMessage e))
        (pprint/pprint e)
        {:err (.getMessage e)}))))

(defn by-state
  "Returns the Places info by parsing the CSV file for the given state name.
   - note: This can be extended to work with more than state name"
  [conf state]
  {:pre [(map? conf) (string? state)]}
  (let [model (fn [item]
                (let [req [:state/code
                           :state/name
                           :district/code
                           :district/name
                           :subdistrict/code
                           :subdistrict/name
                           :village/code
                           :village/name
                           :pincode]]
                  (some->> (not-empty item)
                           (zipmap req))))
        csv-path (str (:resource-path conf) (:pincodes conf))
        reader (io/reader csv-path)
        data (->> (rest (csv/read-csv reader))
                  (filter #(-> % second str/lower-case (= (str/lower-case state)))))
        uc #(util/updatem % (fn [k v]
                              (if (= (name k) "name")
                                (str/upper-case (or v ""))
                                v)))]
    (-> (map (comp uc model) data)
        (util/priority-sort :state/name
                            :district/name
                            :subdistrict/name
                            :village/name))))

(defn fetch->tx!
  "1. Takes the parsed data 
   2. injects **grid and geocodes info** to the villages, by using *google's geocode api*
   3. Transact to XTdb"
  [sys data
   & {interval :interval
      initial  :start
      total    :end
      t?       :trial?
      log      :logger
      pfn      :post-process
      :as      opts}]
  (println "Fetchind data from Google's api for " (count data) " items. And the specification as below: ")
  (pprint/pprint (dissoc opts :data))
  (let [conf    (some-> sys
                        :config
                        :components/place)
        node    (:db/geocodes system)
        datf    (cond->> data
                  (number? total) (take total)
                  (number? initial) (drop initial))
        datc    (count datf)
        datp    (cond->> datf
                  (number? interval) (partition-all interval)
                  (number? interval) (map-indexed #(vector %1 %2)))
        xf      (partial inject-grid-geocode-info conf node)
        tc      (places node :count? true)
        dup!    (atom 0)
        inner!  (fn inner-iter! [!dup-tracker item]
                  (if t?
                    (pprint/pprint item)
                    (let [xfd (not-empty (xf item))]
                      (if-let [res (not-empty (:ok xfd))]
                        (xt/submit-tx-async node [[::xt/put res]])
                        (do
                          (when (str/includes? (str/lower-case (:err xfd)) "already fetched coords")
                            (swap! !dup-tracker inc))
                          (pprint/pprint {:msg "Unexpected Error"
                                          :err (:err xfd)})))
                      (when (fn? log) (log item)))))
        summ    (fn summary! [!dup-tracker iter-info]
                  (let [dbc  (places node :count? true)
                        info (merge {:in-db       @!dup-tracker
                                     :interval    interval
                                     :time        (apply str (take 8 (str (t/time))))
                                     :target      datc
                                     :transacted  (- dbc tc)
                                     :total-in-db dbc}
                                    iter-info)]
                    (if (fn? pfn)
                      (pfn info)
                      (pprint/print-table [info]))))
        worker! (fn execute-inner-iter! [!dup-tracker data iter-info]
                  (doseq [item data]
                    (inner! !dup-tracker item)
                    (summ !dup-tracker iter-info)))]
    (if-not interval
      (worker! dup! datp {:at 0 :iteration (str 0 " __ " (count datp))})
      (doseq [[n part] datp
              :let     [iter-at (+ (* n (or interval 1)) (or initial 0))
                        iter-nx (+ (* (inc n) (or interval 1)) (or initial 0))
                        iterc   (str iter-at " __ " iter-nx)]]
        (worker! dup! part {:index     n
                            :at        iter-at
                            :next      iter-nx
                            :iteration iterc})))
    (println "Fetched and transacted to database!")
    :ok))

(defn parse! [sys opts]
  (println "Starting to parse data")
  (let [conf (some-> sys
                     :config
                     :components/place)
        data (cond
               (:path opts) (let [row (fn parse-row [v]
                                        (-> v
                                            (update-keys (comp
                                                          read-string
                                                          #(str/replace % "-" "/")
                                                          str))
                                            (update-vals (fn [v] (if (number? v)
                                                                   (str (int v))
                                                                   v)))))]
                              (->> (util/parse-spreadsheet (:path opts) (:state opts) :row/fn row)
                                   (sort-by :village/code)))
               :else  (by-state conf (:state opts)))]
    (println "Data parsed successfully. count: " (count data))
    data))