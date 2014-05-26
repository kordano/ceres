(ns ceres.curator
  (:refer-clojure :exclude [assoc! conj! dissoc! ==])
  (:require [clojure.core :as core]
            [com.ashafa.clutch.utils :as utils]
            [com.ashafa.clutch :refer [with-db get-database get-document put-document update-document all-documents]]))


(defn now [] (new java.util.Date))


(def host (or (System/getenv "DB_PORT_5984_TCP_ADDR") "localhost"))


(defn database-url [database]
  (utils/url (utils/url (str "http://" host ":5984")) database))


(defn init-db []
  (get-database (database-url "tweets")))


(defn store-tweet [tweet]
  (put-document (database-url "tweets") tweet))


(defn get-all-tweets []
  (with-db (database-url "tweets")
    (let [ids (map #(:id %) (all-documents))]
      (mapv #(dissoc (get-document %) :_rev) ids))))
