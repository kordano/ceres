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
  (let [uri (str "datomic:mem://ceres-test")]
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
  "Query for user using datomic db"
  [conn id]
  (ffirst
   (d/q '[:find ?r
          :in $ ?id
          :where
          [?r :user/id ?id]]
        (d/db conn) id)))


(defn transact-url
  "Transact url and meta data"
  [conn {:keys [url author ts tweet-id]}]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :url/author author
     :url/address url
     :url/ts (c/to-date ts)
     :url/initial-tweet tweet-id}]))


(defn find-url
  "Query for specific url-string"
  [conn url]
  (d/q '[:find ?r
         :in $ ?url
         :where
         [?r :url/address ?url]]
      (d/db conn) url))


(defn transact-publication
  "Store publication"
  [conn {:keys [user tweet-id mongo-id url ts hashtags pub-type]}]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :publication/author user
     :publication/mongo-tweet mongo-id
     :publication/tweet-id tweet-id
     :publication/url url
     :publication/ts ts
     :publication/type pub-type
     :publication/hashtags hashtags}]))


(defn find-pub-by-tweet-id
  "Query for publication given a specific tweet id"
  [conn id]
  (ffirst
   (d/q '[:find ?r
          :in $ ?id
          :where [?r :publication/tweet-id ?id]]
        (d/db conn) id)))


(defn transact-reaction
  "Transact reaction using two pub ids into datomic"
  [conn publication origin]
  (d/transact
   conn
   [{:db/id (d/tempid :db.part/user)
     :reaction/publication publication
     :reaction/origin origin}]))


(comment

  (def conn (scratch-conn))

  (init-schema conn "schema.edn")

  (time
   (doall
    (pmap #(transact-user conn %) (mc/find-maps @db "users"))))


  (time
   (doseq [entry (mc/find-maps @db "urls")]
     ((comp
       (fn [{:keys [user tweet url ts]}]
         (transact-url conn {:url url :author user :ts ts :tweet-id tweet}))
       (fn [url]
         (update-in url [:user] #(find-user conn (:id (mc/find-map-by-id @db "users" %)))))
       (fn [url]
         (update-in url [:tweet] #(:id (mc/find-map-by-id @db "tweets" %)))))
      entry)))


  (->> (mc/find-maps @db "urls")
       (pmap #(:id (mc/find-map-by-id @db "tweets" (:tweet %))))
       (into #{})
       count)


  (map #(d/q '[:find (count ?r)
              :in $ ?name
              :where
              [?r :url/address ?url]
              [?r :url/author ?uid]
              [?uid :user/screen-name ?name]]
            (d/db conn) %) news-accounts)


  (aprint (d/entity (d/db conn) (d/tempid :db.part/user)))


)
