(ns geocoder.util 
  (:require [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [dk.ative.docjure.spreadsheet :as st]
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

(defn parse-spreadsheet
  [fp sheet
   & {header :header
      hh?    :has-header?
      cfn    :cell/fn
      rfn    :row/fn
      :or    {hh? true
              rfn identity}}]
  (let [res       (some->> (io/file fp)
                           str
                           st/load-workbook-from-file
                           (st/select-sheet sheet)
                           (remove nil?)
                           (map st/cell-seq)
                           (map (partial map st/read-cell)))
        strm      (fn [v] (some-> v str/lower-case
                                  str/trim
                                  (str/replace #"\W" "-")
                                  keyword))
        [hd & rs] (cond
                    (and hh? header)                (cons header (rest res))
                    (and hh? (empty? header))       (cons (mapv strm (first res)) (rest res))
                    (and (not hh?) header)          (cons header res)
                    (and (not hh?) (empty? header)) (cons (range 1 (inc (count (first res)))) res))
        row       (fn [idx item] (cond->>    {(nth hd idx) item}
                                   (fn? cfn) (into [])
                                   (fn? cfn) (apply cfn)))
        tfn       #(dissoc (->> %
                                (map-indexed row)
                                (into {}))
                           nil)]
    (mapv (comp rfn tfn) rs)))

(defonce iso-languages
  (let [model (fn [[_sno lname _family _speakers state iso-639]]
                (let [base {:language/id    (-> iso-639
                                                str/lower-case
                                                (str "-IN")
                                                keyword)
                            :language/name  lname
                            :language/state state}
                      [of ad] (some-> state
                                      str/lower-case
                                      (str/replace "official:" "")
                                      (str/split #"additional:"))
                      m       (fn [s]
                                (some->> (str/split (str s) #"(,|and)")
                                         (map (comp str/trim #(str/replace % #"[^a-zA-Z\s:]"
                                                                           "")))
                                         (remove empty?)
                                         vec))
                      of      (m of)
                      ad      (m ad)
                      loc     {:language/official-states of
                               :language/spoken-states   ad}]
                  (merge base loc)))]
    (some->> (slurp (io/resource "iso_languages.edn"))
             not-empty
             (read-string)
             rest
             (map model)
             (cons {:language/id   :en-IN
                    :language/name "English"})
             (sort-by :language/name)
             (into []))))