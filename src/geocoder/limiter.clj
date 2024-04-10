(ns geocoder.limiter
  "References
   - [Rate limiting algorithm used.](https://en.wikipedia.org/wiki/Token_bucket)"
  (:require [clojure.core.async :refer [chan go >! <! timeout alts!]]))

(defn rate-chan
  "a channel that sends a message at a given rate."
  [burstiness rate]
  (let [c (chan burstiness) ;; bucket size is buffer size
        delta (/ 1000 rate)]
    (go
      (while true
        (>! c :go) ;; send message, will block if bucket is full
        (<! (timeout delta)))) ;; wait a little
    c))

(defn limit-chan
  "a channel that limits another channel by rate."
  [in rc]
  (let [c (chan)]
    (go
      (while true
        (<! rc) ;; wait for message
        (>! c (<! in)))) ;; pass message along
    c))

(defn chan-with-default [in]
  (let [c (chan)]
    (go
      (while true
        ;; take from in, or if not available, pass useful message
        (>! c (alts! [in] :default :rate-exceeded))))
    c))

#_(def rchan
    "Rate Limiting Channel"
    (-> (chan)
        (limit-chan (rate-chan 100 1000))
        (chan-with-default)))