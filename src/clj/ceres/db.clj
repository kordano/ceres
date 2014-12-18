(ns ceres.db
  (:require [datomic.api :as d]
            [clojure.java.io :as io]
            [net.cgrand.enlive-html :as enlive]
            [ceres.collector :refer [db expand-url store-article store-origin custom-formatter news-accounts]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [aprint.core :refer [aprint]])
  (:import datomic.Util))

(def db-uri-base "datomic:free://0.0.0.0:4334")

(defn- scratch-conn
  "Create a connection to an anonymous, in-memory database."
  []
  (let [uri (str "datomic:mem://" (d/squuid))]
    (d/delete-database uri)
    (d/create-database uri)
    (d/connect uri)))


(defn db-conn []
  (let [uri (str db-uri-base "/ceres")]
    (d/create-database uri)
    (d/connect uri)))


(defn read-all
  "Read all forms in f, where f is any resource that can
   be opened by io/reader"
  [f]
  (Util/readAll (io/reader f)))

(defn transact-all
  "Load and run all transactions from f, where f is any
   resource that can be opened by io/reader."
  [conn f]
  (doseq [txd (read-all f)]
    (d/transact conn txd))
  :done)


(defn init-schema [conn path]
  (transact-all conn (io/resource path)))
