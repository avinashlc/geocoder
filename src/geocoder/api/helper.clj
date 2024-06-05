(ns geocoder.api.helper
  (:require [clojure.core.reducers :as r]))

(defn broaden-areas
  "Returns a broadened down set of areas based on the priority or magnitude.
   Here the priority is assumed wrt magnitude.
   Hence, State has the highest and village has the lowest.
   Example:
       if input is [{:state 'gujarat' :district 'botad'} {:state 'gujarat'}]
       it'll narrow it down to #{{:state 'gujarat'}}"
  [areas]
  (let [pull   (fn [k c]
                 (->> areas
                      (r/filter #(and (map? %)
                                      (= (count (dissoc % :place/id)) c)
                                      (not-empty (k %))))
                      (into [])))
        narrow (fn [k base new]
                 (->> new (r/remove (fn [v] (some #(= (k %) (k v)) base)))
                      (into [])))
        st     (pull :state 1)
        dis    (->> (pull :district 2)
                    (narrow :state st))
        subdis (->> (pull :subdistrict 3)
                    (narrow :district dis))
        vil    (->> (pull :village 4)
                    (narrow :subdistrict subdis))]
    (->> (r/flatten [st dis subdis vil])
         (r/remove empty?)
         (into #{}))))

(defn narrow-areas
  "Returns a narrowed down set of areas based on the specification level.
   - Here the priority is assumed wrt specification.
   - Hence, Village has the highest and state has the lowest.
   - Example:
   ```clojure
    (narrow-areas- 
   [{:state \"gujarat\" :district \"botad\" :subdistrict \"byat\"}
    {:state \"gujarat\" :district \"botad\" :subdistrict \"byat\" :village \"byart\"}
    {:state \"gujarat\" :district \"amerli\"}
    {:state \"gujarat\"}
    {:state \"kerela\"}
    {:state \"Tamil nadu\"}
    {:state \"kerela\" :district \"lol\"}
    {:state \"kerela\" :district \"kol\"}])
    
   repl/=> #{{:state \"kerela\", :district \"kol\"}
             {:state \"gujarat\", :district \"botad\", :subdistrict \"byat\", :village \"byart\"}
             {:state \"Tamil nadu\"}
             {:state \"gujarat\", :district \"amerli\"} 
             {:state \"kerela\", :district \"lol\"}}
   ```"
  [areas]
  (let [pull (fn [k c]
               (->> areas
                    (r/filter #(and (map? %)
                                    (= (count (dissoc % :place/id)) c)
                                    (not-empty (k %))))
                    (into [])))
        narrow (fn [k base new]
                 (->> new
                      (remove (fn [v] (some #(= (k %) (k v)) base)))
                      (concat base)))
        vi     (pull :village 4)
        subd    (->> (pull :subdistrict 3)
                     (narrow :subdistrict vi))
        dis    (->> (pull :district 2)
                    (narrow :district subd))
        st (->> (pull :state 1)
                (narrow :state dis))]
    (->> (r/flatten [vi subd dis st])
         (r/remove empty?)
         (into #{}))))

(defn calc-distance-km
  "Returns the great-circle distance between two points on a sphere given their *latitude* and *longitude*.
   - Reference: [Haversine formula](https://en.wikipedia.org//wiki/Haversine_formula) ."
  [coords1 coords2 & {tfn :transform}]
  (let [lat-1      (:latitude coords1)
        lng-1      (:longitude coords1)
        lat-2      (:latitude coords2)
        lng-2      (:longitude coords2)
        valid-lng? #(and (number? %) (<= -180 % 180))
        valid-lat? #(and (number? %) (<= -90 % 90))]
    (when (and (valid-lat? lat-1) (valid-lat? lat-2)
               (valid-lng? lng-1) (valid-lng? lng-2))
      (let [lat-1-rad           (Math/toRadians (Double. lat-1))
            lng-1-rad           (Math/toRadians (Double. lng-1))
            lat-2-rad           (Math/toRadians (Double. lat-2))
            lng-2-rad           (Math/toRadians (Double. lng-2))
            squared-sine        #(Math/pow (Math/sin %) 2)
            first-squared-sine  (squared-sine (/ (- lat-2-rad lat-1-rad) 2))
            second-squared-sine (squared-sine (/ (- lng-2-rad lng-1-rad) 2))
            cosines-product     (* (Math/cos lat-1-rad)
                                   (Math/cos lat-2-rad))
            square-rooted-sum   (Math/sqrt (+ first-squared-sine
                                              (* cosines-product second-squared-sine)))
            earth-radius-km     6371
            haversine-result    (* 2 earth-radius-km (Math/asin square-rooted-sum))]
        (cond (fn? tfn) (tfn haversine-result)
              :else     haversine-result)))))

(defn closest [target coords]
  (let [gc #(select-keys % [:latitude :longitude])
        min (fn [acc next]
              (if (< (calc-distance-km target (gc acc))
                     (calc-distance-km target (gc next)))
                acc
                next))]
    (reduce min coords)))

(defn sort-by-distance [target coords]
  (let [gc #(select-keys % [:latitude :longitude])
        calc #(calc-distance-km (gc target) (gc %))]
    (sort-by calc - coords)))

(comment
  (calc-distance-km
   {:latitude 75.36 :longitude 100.6}
   {:latitude 74.36 :longitude 50.6}
    ;;  :transform (comp parse-double (partial format "%.2f"))
   )

  (sort-by-distance
   {:latitude 20.35 :longitude 78.36}
   [{:latitude 33.35 :longitude 76.36}
    {:latitude 45.99 :longitude 85.336}
    {:latitude 20.35 :longitude 78.36}])

  (narrow-areas [{:state "gujarat" :district "botad"}
                 {:state "gujarat" :district "seinar"}
                ;;  {:state "gujarat"}
                 {:state "tamilnadu" :district "vellore"}
                 {:state "tamilnadu"}])
  :rcf)

(defn prefix [s ps & {r :count}]
  (str (if (int? r)
         (->> (repeat (/ (- r (count s)) (count ps)) ps)
              (apply str))
         ps) s))

(defn fmt-grid-addr-str [addr size]
  (let [suffix #(prefix (str %) "0" :count 4)]
    (-> (str "s" size
             "-x" (-> addr :x suffix)
             "-y" (-> addr :y suffix))
        keyword)))