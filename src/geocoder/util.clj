(ns geocoder.util 
  (:require [clojure.core.reducers :as r]
            [tick.core :as t]))

(defn now->future
  "Returns inst for the interval duration provided from now.
   Params {:minutes || :hours || :seconds || :days @number}
   eg: (now->future :hours 10)
   eg: (now->future :minutes 6)
   "
  [duration-type value]
  (-> (t/now) (t/>> (t/+ (t/new-duration value duration-type))) t/inst))

;; Inst -> Instant
(defn from->future
  "Returns inst for the interval duration provided from time inst.
   Params {:minutes || :hours @number}
   eg: (from->future #inst 'whatevertime' :hours 10)
   "
  [from duration-type value]
  (-> (t/inst from) (t/>> (t/+ (t/new-duration value duration-type))) t/inst))

(defn updatem [m xfn]
  (reduce (fn [acc [k v]]
            (assoc acc k (xfn k v))) {} m))

(defn priority-sort
  "Returns the sorted[asc] col based on the priority of it's map keys.
   EG: (priority-sort [{:a 2 :b 5 :c 9} {:a 2 :b 19 :c 0.9}] :c :a :b)
   => [{:a 2, :b 19, :c 0.9} {:a 2, :b 5, :c 9}]"
  [col & priorities]
  (if (> (count priorities) 0)
    (let [sorter (fn [by res]
                   (->> (sort-by by res)
                        (partition-by by)))
          [x & xs] priorities
          recur- #(apply priority-sort % xs)]
      (->> (sorter x col)
           (r/map recur-)
           r/flatten
           (into [])))
    col))

(defn nsmap->map
  "Removes specified namespace from keys. Takes map and namespace
  keyword as argument. If no namespace is specified, removes all
  namespaces"
  ([m] (reduce-kv (fn [acc k v]
                    (let [new-kw (keyword (name k))]
                      (assoc acc new-kw v)))
                  {} m))
  ([m ns] (reduce-kv (fn [acc k v]
                       (let [new-kw (if (= (name ns) (first (.split (str (symbol k)) "/")))
                                      (keyword (name k))
                                      k)]
                         (assoc acc new-kw v)))
                     {} m)))

(defmacro ok->
  [pred-fn? ok-body
   & {tfn  :fn
      else :else
      :or  {else nil tfn identity}}]
  `(if (~pred-fn? ~ok-body)
     (~tfn ~ok-body)
     ~else))