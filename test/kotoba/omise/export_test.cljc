(ns kotoba.omise.export-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [kotoba.omise :as omise]
            [kotoba.omise.export :as ex]))

(def hours {:mon [["09:00" "18:00"]]})

(def stores
  [(omise/store "st-1" "Kanda, Books" "1-1 Kanda, Tokyo"
                :hours hours :jurisdiction "JPN" :pickup-ready? true)
   (omise/store "st-2" "Quote\"Shop" "addr" :hours hours :status :closed)])

(deftest stores-csv-test
  (let [csv (ex/stores->csv stores)]
    (testing "RFC-4180 quoting for embedded commas and quotes"
      (is (str/includes? csv "\"Kanda, Books\""))
      (is (str/includes? csv "\"Quote\"\"Shop\"")))
    (is (str/starts-with? csv "store_id,name,address,jurisdiction,pickup_ready,status"))))

(deftest pickup-points-csv-test
  (let [csv (ex/pickup-points->csv
             [(omise/pickup-point "pp-1" "st-1" "front" :geo {:lat 35.69 :lng 139.77})])]
    (is (str/includes? csv "35.69"))
    (is (str/starts-with? csv "pickup_id,store,label,lat,lng"))))

(deftest stores-json-test
  (let [json (ex/stores->json stores)]
    (testing "JSON escaping of embedded quotes"
      (is (str/includes? json "Quote\\\"Shop")))
    (is (str/includes? json "\"pickup_ready\":true"))))

(deftest open-report-test
  (let [csv (ex/open-report->csv stores [:mon "10:00"])]
    (is (str/includes? csv "st-1,mon,10:00,yes,yes"))
    (is (str/includes? csv "st-2,mon,10:00,yes,no") "closed store is open by hours but not pickup-available")))
