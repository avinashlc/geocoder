(ns geocoder.scheduler
  (:require [chime.core-async :refer [chime-ch]]
            [chime.core :as chime]
            [tick.core :as t]
            [clojure.core.async :refer [<! <!! go-loop]]
            [geocoder.util :as util]))

(def tasks (atom {}))

(defn run-async-task! [chimes task-id task-fn]
  (swap! tasks assoc task-id
         (<!! (go-loop []
                (when-let [msg (<! chimes)]
                  (task-fn msg)
                  (recur))))))

(defn schedule-async-task!
  "Params: @instant @duration @keyword @fn   
   Example: (schedule-task! #time/instant '2023-02-20T13:41:00Z' #time/duration 'PT24H' :my-task #(println %))
   "
  [time-of-exec interval-duration task-id task-fn]
  (let [chimes (->> interval-duration
                    (chime/periodic-seq time-of-exec)
                    (chime-ch))]
    (run-async-task! chimes task-id task-fn)))

(defn default-err [task-id]
  (fn [err]
    (println {:msg "Error on running task"
              :task/id task-id
              :error err})
    true))

(defn run-task!
  [chimes task-id task-fn
   & {:keys [err-handler]
      :or   {err-handler default-err}}]
  (let [scheduler (chime/chime-at chimes task-fn {:error-handler err-handler})]
    (swap! tasks assoc task-id scheduler)))

(defn schedule-task!
  "### Schedules the task to be executed for the given interval
     - `:execution/time` time.instant, start time of the task
     - `:execution/duration` time.duration, time interval between each execution
     - `:task/id` is the id used to close! the buffer by storing the ch in an atom called tasks
     - `:task/fn` would get the execution time(instant) of the task as it's arguement  
     - Example:
        ```clojure
          (schedule-task! 
            {:execution/time #time/instant \"2023-02-20T13:41:00Z\"
             :execution/interval #time/duration \"PT24H\"
             :task/id :my-task
             :task/fn  #(println %)})    
        ```"
  ([& {exec :execution/time
       interval :execution/interval
       task-id :task/id
       task-fn :task/fn}]
   (if interval
     (-> (chime/periodic-seq exec interval)
         (schedule-task! task-id task-fn))
     (let [exec-at (util/ok-> coll? exec :else [exec])]
       (run-task! exec-at task-id task-fn)))))

(defn cancel-task! [task-id]
  (when-some [task (task-id @tasks)]
    (.close task)))

(comment
  (schedule-task!
   :execution/time (t/instant (util/now->future :seconds 40))
   :execution/interval (t/new-duration 1 :seconds)
   :task/id :my-task-seconds
   :task/fn (fn [exec-time] (println "Good time" exec-time)))
  (cancel-task! :my-task-seconds)
  :rcf)