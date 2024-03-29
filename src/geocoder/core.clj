(ns geocoder.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [geocoder.lib.coords :as gc]
            [geocoder.state :refer [system]]
            [geocoder.util :as util]
            [geocoder.db]
            [juxt.clip.repl :as clip]
            [xtdb.api :as xt]))

(set! *warn-on-reflection* true)

(defn inject-grid-geocode-info
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
        (let [address (->> [(:village/name item)
                            (:subdistrict/name item)
                            (:district/name item)
                            (:state/name item)
                            "India"]
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
                          (update-vals #(if (string? %) (str/upper-case %) %))
                          (update :xt/id #(or % (some->> (:village/code item) (str "vi") (keyword)))))
              valid?  true]
          (println {:msg "modeled tx"
                    :tx tx
                    :valid? valid?})
          (when valid? tx)))
      (catch Exception e (println e)))))

(defn by-state [conf state]
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

(defn csv->tx!
  "1. Takes the parsed csv data 
   2. injects **grid and geocodes info** to the villages, by using *google's geocode api*
   3. Transact to XTdb"
  [conf node csv-data & {interval :interval initial :start total :end t? :trial?}]
  {:pre [(int? initial) (int? total) (int? interval)]}
  (let [data (->> csv-data (take total) (drop initial))
        partitioned (->> (partition interval data)
                         (map-indexed #(vector %1 %2)))
        xf (partial inject-grid-geocode-info conf node)]
    (doseq [[n part] partitioned
            :let [iterc (str (+ (* n interval) initial) "_"
                             (+ (* (inc n) interval) initial))]]
      (println-str  "Iteration at: " iterc)
      (doseq [item part]
        (if t?
          (println item)
          (some->> item xf
                   (conj [::xt/put])
                   vector
                   (xt/submit-tx node)))))))

(defn start! [config]
  (or (when (nil? system)
        (clip/set-init! (fn [] config))
        (when (= (clip/start) :started)
          (alter-var-root #'system (constantly clip/system))
          {:ok "started"}))
      {:error "failed while runnig ```juxt.clip.repl/start```"}))

(defn -main
  "I don't do a whole lot ... yet."
  [& [config-path :as _args]]
  (start! (some->> (or config-path "system.edn")
                   io/resource
                   slurp
                   edn/read-string)))