(ns geocoder.core
  (:gen-class)
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [geocoder.state :refer [system]]
   [geocoder.util :as util]
   [geocoder.transact :as tx]
   [geocoder.web.server :as server]

   [juxt.clip.repl :as clip]))

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
   ["-w" "--web?"   "Start a web server."]
   ["-h" "--help"   "Prints this help summary"]])

(defn start!
  [config & {dev? :dev?}]
  (println "\t\t\t >> Starting the system! <<")
  (if-let [sys (util/initiator! config)]
    (do
      (alter-var-root #'system (constantly clip/system))
      (when dev? (server/dev-start! sys))
      sys)
    (do
      (println "Error Starting the system. TODO! better errors")
      (System/exit 0))))

(defn -main
  [& args]
  ;; {:pre [(.exists (io/file (io/resource (System/getenv "CLIP_SYSTEM"))))]}
  (let [psd  (parse-opts args cli-options)
        opts (:options psd)
        err  (:erros psd)
        summ (:summary psd)
        sysp  (or (System/getenv "CLIP_SYSTEM") "system.edn")]
    (cond
      (not-empty err)     (println (str err "\n" summ))
      (:help opts)        (do (println "CLI SUMMARY:") (println summ "\n"))
      (:web? opts)        (do (println "Starting the web, мадам!")
                              (some->> sysp
                                       io/resource
                                       slurp
                                       edn/read-string
                                       server/start!))
      (not (:state opts)) (println "Error:\n- State name must be provided via -S flag. run with -h flag for more information.")
      :else               (let [sys (some->> sysp
                                             io/resource
                                             slurp
                                             edn/read-string
                                             start!)
                                data (tx/parse! sys opts)]
                            (tx/fetch->tx! sys data opts)
                            (println "\nTotal no of items transacted: " (tx/places (:db/geocodes system) :count? true))))
    (System/exit 0)))