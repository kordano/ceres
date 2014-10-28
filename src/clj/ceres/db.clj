(ns ceres.db
  (:require [datomic.api :as d]
            [clojure.java.io :as io]
            [clj-time.core :as t]
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


(defn transact-hashtag [conn hashtag]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :hashtag/name hashtag}]))

(defn transact-user [conn {:keys [screen_name name id followers_count]}]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :user/screen-name screen_name
     :user/name name
     :user/id id
     :user/followers followers_count}]))

(comment

  (def conn (scratch-conn))

  (init-schema conn "schema.edn")

  (transact-hashtag conn "clojure")

  (transact-user conn {:screen_name "hickey" :name "rich" :id 1234 :followers_count 9000})

  )
