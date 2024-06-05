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
            [throttler.core :as thr]
            [tick.core :as t]
            [xtdb.api :as xt]))

(set! *warn-on-reflection* true)

(defn inject-grid-geocode-info
  "Returns the item with geolocation data merged to it, by making a http request to google's api
   - here the **item** is simply parsed csv entity"
  [conf node item
   & {:keys [validator] :or {validator (constantly true)}}]
  (let [not-incl? #(some-> (util/ok-> string? (:state/name item))
                           str/lower-case
                           (str/includes? %)
                           false?)
        ok?       (and (if node (not (gc/by-id node (or (:xt/id item) item))) true)
                       (not-incl? "english")
                       (not-incl? "state name")
                       (validator item))]
    (try
      (when ok?
        (let [address (->> ["India"
                            (:state/name item)
                            (:district/name item)
                            (:subdistrict/name item)
                            (:village/name item)]
                           (remove nil?)
                           (str/join ", "))
              grid    (gc/indian-address->grid conf address 5)
              tx      (-> item
                          (assoc :latitude (:latitude grid)
                                 :longitude (:longitude grid)
                                 :proximity-grid/x (-> grid :grid/address :x)
                                 :proximity-grid/y (-> grid :grid/address :y)
                                 :proximity-grid/size (:grid/size grid))
                          (update :entity/type #(or % :place))
                          (update-vals #(if (string? %) (str/lower-case %) %))
                          (update :xt/id #(or % (some->> (:village/code item) (str "vi") (keyword)))))
              valid?  true]
          (println {:msg "modeled tx"
                    :tx tx
                    :valid? valid?})
          (when valid? tx)))
      (catch Exception e (println e)))))

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

(defn build->tx!
  "1. Takes the parsed data 
   2. injects **grid and geocodes info** to the villages, by using *google's geocode api*
   3. Transact to XTdb"
  [conf node data & {interval :interval initial :start total :end t? :trial?}]
  ;; {:pre [(int? initial) (int? total) (int? interval)]}
  (let [data (->> data (take total) (drop initial))
        partitioned (->> (partition interval data)
                         (map-indexed #(vector %1 %2)))
        xf (partial inject-grid-geocode-info conf node)
        xf# (thr/throttle-fn xf 30 :second)]
    (doseq [[n part] partitioned
            :let [iterc (str (+ (* n interval) initial) "_"
                             (+ (* (inc n) interval) initial))]]
      (println  "Iteration at: " iterc "\t" (t/time))
      (doseq [item part]
        (if t?
          (pprint/pprint item)
          (some->> item xf#
                   (conj [::xt/put])
                   vector
                   (xt/submit-tx node)))))))

(defn start!
  [config]
  (println "\t\t\t >> Starting the system! <<")
  (or (when (nil? system)
        (clip/set-init! (fn [] config))
        (when (= (clip/start) :started)
          (alter-var-root #'system (constantly clip/system))
          {:ok "started"}))
      {:error "failed while runnig ```juxt.clip.repl/start```"}))

(def cli-options
  [["-p" "--path SPREADSHEET_PATH" "Absoulte Path to the spreadsheet.\n\t\t\t\tIf not provided starts to work with the csv data it fetched."
    :validate [#(.exists (io/file %))
               "Spreadsheet doesn't exist for the given path"]]
   ["-i" "--interval INTERVAL" "Splits the parsed data into mulitple sections based on the given interval"
    :parse-fn #(int (parse-long %))
    :validate [#(<= 0 %) "Must be a number greater than zero"]]
   ["-S" "--state STATE" "State name in english"]
   ["-s" "--start START_FROM" "Get items from this position of the parsed data"
    :parse-fn #(-> % parse-long int)]
   ["-e" "--end END_AT" "Stop Gettting items at this position of the parsed data"
    :parse-fn #(-> % parse-long int)]
   ["-t" "--trial?" "Does a trial run on the parsed data"]
   ["-h" "--help"   "Prints this help summary"]])

(defn -main
  [& args]
  {:pre [(.exists (io/file (io/resource "system.edn")))]}
  (let [{opts :options
         sum  :summary
         err :errors} (parse-opts args cli-options)]
    (start! (some->> (io/resource "system.edn")
                     slurp
                     edn/read-string))
    (cond
      (not-empty err) (println (str err "\n" sum))
      (:help opts)    (do  (println "CLI SUMMARY:") (println sum "\n"))
      :else           (println opts))
    (System/exit 0)))