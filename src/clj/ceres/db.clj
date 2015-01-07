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


(defn transact-user
  "doc-string"
  [conn {:keys [screen_name id created_at]}]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :user/id (long id)
     :user/screen-name screen_name
     :user/created (c/to-date created_at)}]))


(defn find-user
  "Query for user"
  [db id]
  (ffirst
   (d/q '[:find ?r
          :in $ ?id
          :where
          [?r :user/id ?id]]
        db id)))


(defn transact-url
  "Transact url and meta data"
  [conn {:keys [url author ts mongo-id]}]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :url/author author
     :url/address url
     :url/ts ts
     :url/initial-tweet mongo-id}]))


(defn find-url
  "Query for specific url-string"
  [db url]
  (d/q '[:find ?r
         :in $ ?url
         :where
         [?r :url/address ?url]]
       db url))


(defn transact-publication
  "Transact publication into datomic"
  [conn {:keys [user mongo-id url ts]}]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :publication/author user
     :publication/tweet mongo-id
     :publication/url url
     :publication/ts ts}])
  )


(comment

  (def conn (scratch-conn))

  (def ddb (d/db conn))

  (init-schema conn "schema.edn")

  (time
   (doall
    (pmap #(transact-user conn %) (mc/find-maps @db "users"))))




)
