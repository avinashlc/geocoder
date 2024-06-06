(ns geocoder.api.coords 
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [com.rpl.specter :as spr]
            [geocoder.api.helper :as helper]
            [geocoder.util :as util]
            [malli.core :as m]
            [throttler.core :as thr]
            [xtdb.api :as xt]))

(def coords-spec
  [:map
   [:latitude double?]
   [:longitude double?]])

(def location-spec
  [:map
   [:bounds [:map
             [:northeast coords-spec]
             [:southwest coords-spec]
             [:northwest coords-spec]
             [:southeast coords-spec]]]
   [:address {:optional true} [:string {:min 1}]]
   [:size int?]
   [:location coords-spec]
   [:x double?]
   [:y double?]])

(def grid-spec
  [:map
   [:grid/end {:optional true} coords-spec]
   [:grid/size int?]
   [:grid/address [:map [:x int?] [:y int?]]]
   [:grid/center coords-spec]
   [:grid/origin {:optional true} coords-spec]])

(defn coords? [coords]
  (m/validate coords-spec coords))

(defn location? [data]
  (m/validate location-spec data))

(defn grid? [data]
  (m/validate grid-spec data))

(defn x-or-y [direction]
  (case direction
    :up :y
    :right :x
    :left :x
    :down :y
    nil))

(defn rename-loc-keys
  "Returns a data with renamed keys of location map.
  From :lat -> :latitude
       :lng -> :longitude"
  [loc]
  (spr/transform
   (spr/recursive-path [] p
                       [(spr/walker :lat) spr/MAP-KEYS])
   (fn [k]
     (case k
       :lat :latitude
       :lng :longitude
       k))
   loc))

(defn -geocode-get-req! [config params]
  (let [geocode-url (:geocode-url config)
        api-key (:google-api config)
        res (client/get geocode-url {:query-params (assoc params :key api-key)})]
    (-> res :body json/read-json)))

(def geocode-get-req!
  "Limits the amounts of request per seconds to 30"
  (thr/throttle-fn -geocode-get-req! 
                   30 
                   :second))

(defn geometry!
  "Returns a detail info about the address by making a get request to google's goecode endpoint"
  [config address]
  (->> {:address address}
       (geocode-get-req! config)
       :results first :geometry))

(defn location->coordiantes
  "Fetches co-ordinates info for the given location and returns {:lat,:lng}
   Parameters: location-name (req),
   e.g: (location->coordiantes 'Gujarat') -> {:lat,:lng},
   "
  [config location]
  (when-let [res (:location (geometry! config location))]
    (rename-loc-keys res)))

;; TODO: get coords for all villages [gu]
;; TODO: coordiantes->nearby-villages [4 closest]

;; Load from excel and compare
;; Load in xtdb and query

(defn coordiantes->location
  "Fetches location info for the given co-ordinates and returns the address of that co-ordiantes.
   Parameters: co-rodinates (req), location-type (opt).
   The Optional parameter is just for testing.
   The default loction type 'administrative_area_level_2' -
   - is tested to provide better results for our use case, such as district level details.
   Other location types :
      ['street_address' 'plus_code' 'route' 'postal_code'
       'administrative_area_level_2' 'administrative_area_level_1' 'country']

   e.g: (coordiantes->location {:latitude 20.127954, :longitude 68.1623859})
   "
  [config coords & {get? :get-all? lt :location/type}]
  (let [coords-str (str (:latitude coords) "," (:longitude coords))
        res (geocode-get-req! config {:latlng coords-str})]
    (if get?
      (:results res)
      (->> res :results
           (filter #(= (-> % :types first) (or lt "administrative_area_level_2")))
           first :formatted_address))))

(defn area-boundary
  "Returns the boundaries for all the axis/direction in a given map of two diagonal bounds.
   takes :northeast and :southwest as input bounds"
  [{ne :northeast sw :southwest :as bounds}]
  (assoc bounds
         :northwest {:latitude (:latitude ne)
                     :longitude (:longitude sw)}
         :southeast {:latitude (:latitude sw)
                     :longitude (:longitude  ne)}))

(defn get-location-bounds
  "Fetches co-ordinates info for the given location and returns {:lat,:lng}.
   Parameters: location-name
   e.g: (get-bounds 'Gujarat') -> {:northwest :northeast :southwest :southeast}
  "
  [config location-name]
  (when-let [res (:bounds (geometry! config location-name))]
    (-> res rename-loc-keys area-boundary)))

(defn shift-latitude
  "Returns co-ordinates with latitude shifted of given distance in kms"
  [origin distance-km]
  {:pre [(coords? origin)
         (number? distance-km)]
   :post [coords?]}
  (let [;; degree-in-km 110.574
        degree-in-km 110.6]
    (update origin :latitude #(+ % (/ distance-km degree-in-km)))))

(defn shift-longitude
  "Returns co-ordinates with longitude shifted of given distance in kms"
  [origin distance-km]
  {:pre [(coords? origin)
         (number? distance-km)]
   :post [coords?]}
  (let [;;  degree-in-km 102.018
        degree-in-km 111.320]
    (update origin :longitude
            #(+ % (->> (:latitude origin)
                       Double. Math/toRadians Math/cos
                       (* degree-in-km)
                       (/ distance-km))))))

(defn shift-coordinates
  "Shifts given location co-ordinates by distance in kms in requested direction."
  [origin distance-km direction]
  (case direction
    :up (shift-latitude origin distance-km)
    :right (shift-longitude origin distance-km)
    :down (shift-latitude origin (- distance-km))
    :left (shift-longitude origin (- distance-km))))

(defn shift-for-all-directions
  "Shifts given location co-ordinates by distance in kms in requested direction."
  [origin distance-km]
  {:up (shift-coordinates origin distance-km :up)
   :right (shift-coordinates origin distance-km :right)
   :down (shift-coordinates origin (- distance-km) :down)
   :left (shift-coordinates origin (- distance-km) :left)})

(defn shift-grid
  ([{x :x y :y} direction unit]
   (-> (case direction
         :up    {:x x :y (+ unit y)}
         :right {:x (+ unit x) :y y}
         :down  {:x x :y (- y unit)}
         :left  {:x (- x unit) :y y}
         nil)
       (update-vals #(util/ok-> (partial < 0) % :else 0))))
  ([grid direction] (shift-grid grid direction 1)))

(defn expand-grid
  ([addr unit]
   (let [{x :x y :y} (util/nsmap->map addr)
         rev (fn [start dist]
               (concat (range (- start dist) start)
                       (range start (+ start (+ 1 dist)))))
         xs  (rev x unit)
         ys  (rev y unit)]
     (for [x xs y ys]
       {:x x :y y})))
  ([addr] (expand-grid addr 1)))

;; TODO: Rename fn
(defn grid-info-for-point
  "Returns a map of info about the grid from a point wrt size.
   The point here is always assumed to be the top right end of the grid."
  [top-right address size]
  {:grid/end     top-right
   :grid/size    size
   :grid/address address
   :grid/origin  (-> (shift-coordinates top-right size :down)
                     (shift-coordinates size :left))
   :grid/center  (-> (shift-coordinates top-right (/ size 2) :down)
                     (shift-coordinates (/ size 2) :left))})

(defn grid-cell-address
  "The idea is to create a grid of give size (in kms) and get grid address
   of the co-ordinates.

   Parameters:
   origin: starting point of the grid. The bottom-left/south-west co-ordinate to start the grid from.
   target: the location of which we want the grid address
   size: size of grid (in kms)
   address: The current grid address. (This is a recursive function and address is used as an accumulator.)

   The strategy is simple. We start from origin and move up by the size incrementing the y co-ordinates,
   and checkin if we have travelled beyond the target latitude. Once we have arrived at the target latitude,
   we do the same for longitude. Once we have arrived at the target grid cell retrun:
   :grid/address: x and y address of target location in the grid. e.g {:x 30 :y 14}
   :grid/origin: origin co-ordinates of the target grid cell
   :grid/center: center co-ordinates of the target grid cell
   :grid/end: end co-ordinates of the target grid cell
   "
  [origin target size address]
  (let [shift (fn [direction]
                (-> (shift-coordinates origin size direction)
                    (grid-cell-address
                     target size (update address (x-or-y direction) inc))))
        low? #(> (% target) (% origin))]
    (cond
      (low? :latitude) (shift :up)
      (low? :longitude) (shift :right)
      :else (grid-info-for-point origin address size))))

(defn grid-cell-addres-to-location
  "The idea is to create a grid of given size (in kms) and grid address
   of certain location, to get the co-ordinates of that grid in detail.

   Parameters:
   origin: starting point of the grid. The bottom-left/south-west co-ordinate to start the grid from.
   target-address: the address of the location for which we want the grid co-ordiantes., e.g: {:x 5 :y 15}
   size: size of grid (in kms)
   address: The current grid address. (This is a recursive function and address is used as an accumulator.)

   The strategy is simple. We start from origin and move up by the size incrementing the y co-ordinates,
   and checkin if we have travelled beyond the target latitude. Once we have arrived at the target latitude,
   we do the same for longitude. Once we have arrived at the target grid cell retrun:
   :grid/address: x and y address of target location in the grid. e.g {:x 30 :y 14}
   :grid/origin: origin co-ordinates of the target grid cell
   :grid/center: center co-ordinates of the target grid cell
   :grid/end: end co-ordinates of the target grid cell
   "
  [origin target-address size current-address]
  (let [shift (fn [direction]
                (-> (shift-coordinates origin size direction)
                    (grid-cell-addres-to-location
                     target-address size (update current-address (x-or-y direction) inc))))
        move? #(> (% target-address) (% current-address))]
    (cond
      (move? :y) (shift :up)
      (move? :x) (shift :right)
      :else      (grid-info-for-point origin current-address size))))

(defn address->grid
  [data target-address size]
  {:pre [(location? data)]}
  (let [southwest (-> data :bounds :southwest)]
    (when (and (>= (:x data) (:x target-address))
               (>= (:y data) (:y target-address)))
      (grid-cell-addres-to-location southwest target-address size {:x 0 :y 0}))))

(defn coords->grid
  [data target size]
  {:pre [(location? data)]}
  (let [bounds (:bounds data)]
    (when (and (<= (:latitude target) (-> bounds :northeast :latitude))
               (<= (:longitude target) (-> bounds :southeast :longitude)))
      (merge (grid-cell-address (:southwest bounds) target size {:x 0 :y 0}) target))))

(defn fmt-grid-addr-str [addr size]
  (let [suffix #(let [addr-str (str %)]
                  (if (= (count addr-str) 1)
                    (str "00" addr-str)
                    (if (= (count addr-str) 2)
                      (str "0" addr-str)
                      (when (>= (count addr-str) 3)
                        addr-str))))]
    (str "gu-s" size
         "-x" (-> addr :x suffix)
         "-y" (-> addr :y suffix))))

(defn bounds->gridc
  "Returns a map of {:x number :y number} by taking the
   boundaries of a location and calculates the number of grids associated within it"
  [bounds size]
  (let [gc (fn grid-count [from to]
             (-> (from bounds)
                 (helper/calc-distance-km (to bounds))
                 (/ size)
                 Math/ceil))]
    {:x (gc :northwest :northeast)
     :y (gc :southeast :northeast)}))

(defn get-location-info
  "Returns general location info about location.
   Paramater : size (in kms)
   size param is required to get the limit of x and y grid address of location for the given size.
   e.g (get-location-info 12) ->  {:location, :bounds, :x, :y}"
  [config address
   & {size :size
      :or  {size 5}}]
  {:post [location?]}
  (let [{bounds :bounds
         loct   :location} (-> (geometry! config address)
                               (select-keys [:bounds :location])
                               (update-vals rename-loc-keys)
                               (update :bounds area-boundary))
        dist               (fn [from to]
                             (-> (from bounds)
                                 (helper/calc-distance-km (to bounds))))
        gc                 (fn grid-count [from to]
                             (-> (dist from to)
                                 (/ size)
                                 Math/ceil))]
    {:address  address
     :bounds   bounds
     :location loct
     :size     size
     :x-km     (dist :northwest :northeast)
     :y-km     (dist :southeast :northeast)
     :x        (gc :northwest :northeast)
     :y        (gc :southeast :northeast)}))

(defn india-coords
  "Returns general location info about India.
   Paramater : size (in kms)
   size param is required to get the limit of x and y grid address of India for the given size.
   e.g (India-coords 12) ->  {:location, :bounds, :x, :y}
   "
  [size]
  (let [bounds {:northeast {:latitude  35.6745457
                            :longitude 97.39535869999999}
                :southwest {:latitude  6.4626999
                            :longitude 68.1097}
                :northwest {:latitude  35.6745457
                            :longitude 68.1097}
                :southeast {:latitude  6.4626999
                            :longitude 97.39535869999999}}
        gc     (fn grid-count [from to]
                 (-> (from bounds)
                     (helper/calc-distance-km (to bounds))
                     (/ size)
                     Math/ceil))]
    {:bounds   bounds
     :location {:latitude  20.593684
                :longitude 78.96288}
     :size     size
     :x        (gc :northwest :northeast)
     :y        (gc :southeast :northeast)}))

(defn indian-address->grid [config address size]
  {:pre [(string? address) (int? size)]
   :post [grid?]}
  (when-let [target (->> address
                         (location->coordiantes config)
                         (util/ok-> coords?))]
    (coords->grid (india-coords size) target size)))

(defn indian-coords->grid [target size]
  {:pre  [(coords? target) (int? size)]
   :post [grid?]}
  (coords->grid (india-coords size) target size))

(defn by-id [node xtid]
  (xt/entity (xt/db node) xtid))

;; EXAMPLES
(comment

  ;; (get-location-info @lc.place.interface/config "gujarat")

  ;; (->> "Dela, Mahesana,	Mahesana, Gujarat, India"
  ;;      (location->coordiantes @lc.place.interface/config)
  ;;      (util/ok-> coords?))
  ;; (india-coords 5)
  ;; (india-grid-cell-address-to-location {:x 27 :y 21.5} 5)
  ;; (india-grid-cell-address-to-location {:x 27 :y 21.5} 5)
  ;; (india-grid-cell-address-to-location {:x 54 :y 43} 5)
  ;; (indian-address->grid @lc.place.interface/config "TEKNAR, DANTEWADA, DANTEWADA, CHHATTISGARH, India" 5)

  "Dela, Mahesana,	Mahesana, Gujarat, India"

  23.623677,72.433212
  23.61911943	72.44891721
  :rcf)