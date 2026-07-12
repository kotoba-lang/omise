(ns kotoba.omise.export
  "Operator-facing export for a courier actor's store directory.

  Renders stores and pickup points to CSV and JSON for audit and
  downstream reporting. Pure data → text: no network."
  (:require [clojure.string :as str]
            [kotoba.omise :as omise]))

(defn- csv-cell [v]
  (let [s (str (if (nil? v) "" v))]
    (if (re-find #"[\",\n]" s)
      (str "\"" (str/replace s "\"" "\"\"") "\"")
      s)))

(defn- csv-row [vals] (str/join "," (map csv-cell vals)))

(defn- json-str [v]
  (-> (str (if (nil? v) "" v))
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn stores->csv [stores]
  (str/join "\n"
    (cons (csv-row ["store_id" "name" "address" "jurisdiction" "pickup_ready" "status"])
          (for [s stores]
            (csv-row [(:omise/id s)
                      (:omise/name s)
                      (:omise/address s)
                      (or (:omise/jurisdiction s) "")
                      (if (:omise/pickup-ready? s) "yes" "no")
                      (name (:omise/status s))])))))

(defn pickup-points->csv [points]
  (str/join "\n"
    (cons (csv-row ["pickup_id" "store" "label" "lat" "lng"])
          (for [p points]
            (csv-row [(:pickup/id p)
                      (:pickup/store p)
                      (or (:pickup/label p) "")
                      (get-in p [:pickup/geo :lat] "")
                      (get-in p [:pickup/geo :lng] "")])))))

(defn stores->json [stores]
  (str "["
       (str/join ","
                 (for [s stores]
                   (str "{\"store_id\":\"" (json-str (:omise/id s)) "\","
                        "\"name\":\"" (json-str (:omise/name s)) "\","
                        "\"address\":\"" (json-str (:omise/address s)) "\","
                        "\"pickup_ready\":" (if (:omise/pickup-ready? s) "true" "false") ","
                        "\"status\":\"" (name (:omise/status s)) "\"}")))
       "]"))

(defn open-report->csv
  "Open/closed report for every store at a caller-supplied moment."
  [stores [day hhmm]]
  (str/join "\n"
    (cons (csv-row ["store_id" "day" "time" "open" "pickup_available"])
          (for [s stores]
            (csv-row [(:omise/id s)
                      (name day)
                      hhmm
                      (if (omise/open-at? s day hhmm) "yes" "no")
                      (if (omise/pickup-available? s day hhmm) "yes" "no")])))))
