(ns geocoder.places 
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]            
            [clojure.string :as str]))

(defn pull-places! [conf]
  (println :msg "Pulling Places file"
           :conf conf)
  (try
    (let [zipname (-> (str/split (:lgd conf) #"/") last)
          tmppath (str "/tmp/" zipname)
          folder  (str (-> (str/split tmppath #"\.") first) "/")
          cp      #(io/copy (io/file %) (io/file %2))]
      (with-open [in  (io/input-stream (:lgd conf))
                  out (io/output-stream tmppath)]
        (io/copy in out))
      (sh/sh "7za" "e" "-y" tmppath (str "-o" folder))
      (when-not (.exists (io/file (:resource-path conf)))
        (.mkdir (io/file (:resource-path conf))))
      ;; (cp (str folder (:villages conf)) (str (:resource-path conf) (:villages conf)))
      (cp (str folder (:pincodes conf)) (str (:resource-path conf) (:pincodes conf)))
      (println :msg "Copying csv & Starting again")
      :ok)
    (catch Exception e
      (do
        (println :msg "Error Pulling file"
                 :conf conf
                 :err e)
        :error))))

(defn init-india! [config node]
  (println "Initializing Places component")
  (when-let [conf (not-empty config)]
    (if-not (.exists (io/file (str (:resource-path conf) (:pincodes conf))))
      (when (= :ok (pull-places! conf))
        (init-india! conf node))
      :ok)
    (println "Successfully Initialized Places component\n")))