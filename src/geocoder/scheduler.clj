(ns geocoder.scheduler
  (:require [chime.core :as chime]
            [tick.core :as t]
            [geocoder.util :as util]))

(def tasks (atom {}))

(defn default-err [task-id]
  (fn [err]
    (println {:msg "Error on running task"
              :task/id task-id
              :error err})
    true))

(defn background-task!
  [task-id task-fn &
   {:keys [err-handler execution/time]
    :or   {err-handler default-err
           time (t/inst)}}]
  (let [scheduler (chime/chime-at [time] task-fn {:error-handler err-handler})]
    (swap! tasks assoc task-id scheduler)))

(defn cancel-task! [task-id]
  (when-some [task (task-id @tasks)]
    (.close task)))

(comment
  (background-task!
   :execution/time (t/instant (util/now->future :seconds 40)) ;; optional  
   :task/id :my-task-seconds
   :task/fn (fn [exec-time] (println "Good time" exec-time)))
  (cancel-task! :my-task-seconds)
  :rcf)