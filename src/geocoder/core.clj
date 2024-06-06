(ns geocoder.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [geocoder.api.coords :as gc]
            [geocoder.db]
            [geocoder.scheduler]
            [geocoder.state :refer [system]]
            [geocoder.util :as util]
            [juxt.clip.repl :as clip]
            [tick.core :as t]
            [xtdb.api :as xt]))

(set! *warn-on-reflection* true)

(defonce cli-options
  [["-p" "--path SPREADSHEET_PATH" "Absoulte Path to the spreadsheet.\n\t\t\t\tIf not provided starts to work with the csv data it fetched."
    :validate [#(.exists (io/file %))
               "Spreadsheet doesn't exist for the given path"]]
   ["-i" "--interval INTERVAL" "Splits the parsed data into mulitple sections based on the given interval"
    :parse-fn #(int (parse-long %))
    :validate [#(<= 0 %) "Must be a number greater than zero"]]
   ["-S" "--state STATE" "State name in english"
    :parse-fn (comp str/trim str/lower-case str)
    :validate [#(false? (empty? (str %))) "State must be provided"]]
   ["-s" "--start START_FROM" "Get items from this position of the parsed data"
    :parse-fn #(-> % parse-long int)]
   ["-e" "--end END_AT" "Stop Gettting items at this position of the parsed data"
    :parse-fn #(-> % parse-long int)]
   ["-t" "--trial?" "Does a trial run on the parsed data"]
   ["-h" "--help"   "Prints this help summary"]])

(defn start!
  [config]
  (println "\t\t\t >> Starting the system! <<")
  (if-let [sys (not-empty
                (when (nil? system)
                  (clip/set-init! (fn [] config))
                  (when (= (clip/start) :started)
                    (alter-var-root #'system (constantly clip/system))
                    system)))]
    sys
    (do
      (println "Error Starting the system. TODO! better errors")
      (System/exit 0))))

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
      c? (ffirst res)
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
        (println "Already fetched coords for " (:xt/id in-db))
        (when ok?
          (let [address (->> ["India"
                              (:state/name item)
                              (:district/name item)
                              (:subdistrict/name item)
                              (:village/name item)]
                             (remove nil?)
                             (str/join ", "))
                grid    (gc/indian-address->grid conf address 5)]
            (some-> item
                    (assoc :latitude (:latitude grid)
                           :longitude (:longitude grid)
                           :proximity-grid/x (-> grid :grid/address :x)
                           :proximity-grid/y (-> grid :grid/address :y)
                           :proximity-grid/size (:grid/size grid))
                    (update :entity/type #(or % :place))
                    (update-vals #(if (string? %) (str/lower-case %) %))
                    (update :xt/id #(or % (:village/code item)))))))
      (catch Exception e
        (println "Someting Went wrong " (.getMessage e))
        (pprint/pprint e)))))

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
  [sys data & {interval :interval initial :start total :end t? :trial? :as opts}]
  (println "Fetchind data from Google's api for " (count data) " items. And the specification as below: ")
  (pprint/pprint opts)
  (let [conf (some-> sys
                     :config
                     :components/place)
        node (:db/geocodes system)
        datf (cond->> data
               (number? total) (take total)
               (number? initial) (drop initial))
        datc (count datf)
        datp (cond->> datf
               (number? interval)       (partition-all interval)
               (not (number? interval)) vector
               :always                  (map-indexed #(vector %1 %2)))
        xf   (partial inject-grid-geocode-info conf node)
        tc   (places node :count? true)
        ;; xf#  (thr/throttle-fn xf 30 :second)
        ]
    (doseq [[n part] datp
            :let     [iterc (str (+ (* n (or interval 1)) (or initial 0)) " __ "
                                 (+ (* (inc n) (or interval 1)) (or initial 0)))
                      dbc   (places node :count? true)
                      info  {:iteration   iterc
                             :time        (str (t/time))
                             :target      datc
                             :transacted  (- dbc tc)
                             :total-in-db dbc}]]
      (pprint/print-table [info])
      (doseq [item part]
        (if t?
          (pprint/pprint item)
          (some->> item xf
                   (conj [::xt/put])
                   vector
                   (xt/submit-tx-async node)))))
    (println "Fetched and transacted to database!")))

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

(defn -main
  [& args]
  ;; {:pre [(.exists (io/file (io/resource (System/getenv "CLIP_SYSTEM"))))]}
  (let [psd  (parse-opts args cli-options)
        opts (:options psd)
        err  (:erros psd)
        summ (:summary psd)]
    (cond
      (not-empty err)     (println (str err "\n" summ))
      (:help opts)        (do (println "CLI SUMMARY:") (println summ "\n"))
      (not (:state opts)) (println "Error:\n- sState name must be provided via -S flag. run with -h flag for more information.")
      :else               (let [sys (some->> (System/getenv "CLIP_SYSTEM")
                                             io/resource
                                             slurp
                                             edn/read-string
                                             start!)
                                data (parse! sys opts)]
                            (fetch->tx! sys data opts)
                            (println "Total no of items transacted: " (places (:db/geocodes system) :count? true))))
    (System/exit 0)))