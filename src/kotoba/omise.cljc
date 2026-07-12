(ns kotoba.omise
  "Stores (お店), opening hours and pickup points — pure data contracts.

  A kotoba-lang capability library for the cloud-itonami-5320 (community
  last-mile courier) open business. No network, no I/O, no clock access —
  the caller always passes the day/time to check, so every function is a
  pure, deterministic contract. Models the records a merchant-facing
  courier operator keeps: store records, weekly opening-hours windows,
  open-at? checks, pickup points, and geo distance between two points.

  Portable (.cljc) across JVM / ClojureScript / SCI / GraalVM."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Opening hours — weekly windows, pure time arithmetic (no clock)
;; ---------------------------------------------------------------------------

(def days
  "Weekday keywords accepted in an hours map."
  #{:mon :tue :wed :thu :fri :sat :sun})

(defn hhmm->minutes
  "\"HH:MM\" → minutes since midnight, or nil when malformed/out of range."
  [s]
  (when (string? s)
    (when-let [[_ h m] (re-matches #"(\d{2}):(\d{2})" s)]
      (let [h #?(:clj (Long/parseLong h) :cljs (js/parseInt h 10))
            m #?(:clj (Long/parseLong m) :cljs (js/parseInt m 10))]
        (when (and (<= 0 h 23) (<= 0 m 59))
          (+ (* 60 h) m))))))

(defn hhmm-valid? [s] (some? (hhmm->minutes s)))

(defn window-valid?
  "A window is [\"HH:MM\" \"HH:MM\"] with open strictly before close."
  [w]
  (and (vector? w) (= 2 (count w))
       (let [[o c] (map hhmm->minutes w)]
         (and o c (< o c)))))

(defn hours-valid?
  "An hours map is {day [[open close] ..]} — every key a weekday keyword,
  every value a vector of valid windows. An empty map (always closed) is
  valid."
  [hours]
  (and (map? hours)
       (every? days (keys hours))
       (every? (fn [ws] (and (vector? ws) (every? window-valid? ws)))
               (vals hours))))

;; ---------------------------------------------------------------------------
;; Store
;; ---------------------------------------------------------------------------

(defn geo-valid?
  "{:lat .. :lng ..} within WGS-84 ranges."
  [{:keys [lat lng] :as g}]
  (and (map? g) (number? lat) (number? lng)
       (<= -90.0 lat 90.0) (<= -180.0 lng 180.0)))

(defn store
  "Construct a store record. status is one of :active/:suspended/:closed.
  Returns nil when the status, hours or geo is malformed."
  [id name address & {:keys [geo hours jurisdiction pickup-ready? status]}]
  (let [st (or status :active)
        hrs (or hours {})]
    (when (and (contains? #{:active :suspended :closed} st)
               (hours-valid? hrs)
               (or (nil? geo) (geo-valid? geo)))
      {:omise/id            id
       :omise/name          name
       :omise/address       address
       :omise/geo           geo
       :omise/hours         hrs
       :omise/jurisdiction  jurisdiction
       :omise/pickup-ready? (boolean pickup-ready?)
       :omise/status        st})))

(defn active? [s] (= :active (:omise/status s)))

(defn open-at?
  "Is the store's hours table open at `day` (weekday keyword) and `hhmm`?
  Pure — the caller supplies the moment; a store with no windows for the
  day (or an invalid time) is closed. Open is inclusive, close exclusive."
  [store day hhmm]
  (boolean
   (when-let [t (hhmm->minutes hhmm)]
     (some (fn [[o c]]
             (let [om (hhmm->minutes o) cm (hhmm->minutes c)]
               (and om cm (<= om t) (< t cm))))
           (get (:omise/hours store) day)))))

(defn pickup-available?
  "Can a courier pick up at this store at `day`/`hhmm`? — the single
  question the courier governor asks: the store must be :active, flagged
  pickup-ready, and open at that moment."
  [store day hhmm]
  (and (active? store)
       (true? (:omise/pickup-ready? store))
       (open-at? store day hhmm)))

;; ---------------------------------------------------------------------------
;; Pickup point
;; ---------------------------------------------------------------------------

(defn pickup-point
  "Construct a pickup-point record attached to a store. Returns nil when
  either id is blank or the geo is malformed."
  [id store-id label & {:keys [geo]}]
  (when (and (string? id) (not (str/blank? id))
             (string? store-id) (not (str/blank? store-id))
             (or (nil? geo) (geo-valid? geo)))
    {:pickup/id    id
     :pickup/store store-id
     :pickup/label label
     :pickup/geo   geo}))

;; ---------------------------------------------------------------------------
;; Geo distance
;; ---------------------------------------------------------------------------

(defn- deg->rad [d] (* d (/ Math/PI 180.0)))

(defn distance-km
  "Haversine great-circle distance in km between two {:lat :lng} points,
  or nil when either point is malformed."
  [g1 g2]
  (when (and (geo-valid? g1) (geo-valid? g2))
    (let [r 6371.0088
          dlat (deg->rad (- (:lat g2) (:lat g1)))
          dlng (deg->rad (- (:lng g2) (:lng g1)))
          a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2)))
               (* (Math/cos (deg->rad (:lat g1)))
                  (Math/cos (deg->rad (:lat g2)))
                  (Math/sin (/ dlng 2)) (Math/sin (/ dlng 2))))]
      (* 2 r (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1.0 a)))))))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn validate-store
  "Return a validation result for a candidate store record."
  [s]
  (cond
    (not (map? s))                        {:omise/valid? false :omise/error :not-a-map}
    (str/blank? (str (:omise/id s)))      {:omise/valid? false :omise/error :missing-id}
    (not (hours-valid? (:omise/hours s))) {:omise/valid? false :omise/error :malformed-hours}
    (and (:omise/geo s)
         (not (geo-valid? (:omise/geo s)))) {:omise/valid? false :omise/error :malformed-geo}
    (not (contains? #{:active :suspended :closed} (:omise/status s)))
    {:omise/valid? false :omise/error :unknown-status}
    :else {:omise/valid? true}))
