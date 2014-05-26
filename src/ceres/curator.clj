(ns ceres.curator
  (:refer-clojure :exclude [assoc! conj! dissoc! ==])
  (:require [clojure.core :as core]
            [com.ashafa.clutch.utils :as utils]
            [com.ashafa.clutch :refer [save-view view-server-fns with-db get-database get-document put-document update-document all-documents]]))


(defn now [] (new java.util.Date))


(def host (or (System/getenv "DB_PORT_5984_TCP_ADDR") "localhost"))


(defn database-url
  "assemble database url using env variables if necessary"
  [database]
  (utils/url (utils/url (str "http://" host ":5984")) database))


(defn init-db
  "Initializes the database if it is non-existent"
  []
  (get-database (database-url "tweets")))


(defn store-tweet
  "Stores the tweet"
  [tweet]
  (put-document (database-url "tweets") tweet))


(defn get-all-tweets []
  (with-db (database-url "tweets")
    (let [ids (map #(:id %) (all-documents "tweets"))]
      (mapv #(dissoc (get-document %) :_rev) ids))))

(defn get-tweets
  "Get all tweets posted by a specific user"
  [username]
  (filter #(= (-> % :user :screen_name) username) (get-all-tweets)))


(comment

  (with-db (database-url "tweets")
    (->> (all-documents)
         (mapv :id)
         (mapv #(get-document %))
         (filter #(= "olibrecht" (-> % :user :screen_name)))
         time))

  (with-db "tweets"
  (save-view "test1"
    (view-server-fns :cljs
      {:your-view-name {:map (fn [doc]
                               (js/emit (aget doc "_id") nil))}})))


  (defn create-index
    [news-accounts]
    (->> (map
          (fn [account]
            (vec [account
                  (->> (get-news account)
                       (map #(vec [(:id %)
                                   (into {} [[:tweet %]
                                             [:retweets {}]
                                             [:replies {}]
                                             [:shared {}]])]))
                       vec
                       (into {}))]))
          news-accounts)
         vec
         (into {}))))
