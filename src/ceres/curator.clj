(ns ceres.curator
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.string :refer [split]]
            [monger.conversion :refer [from-db-object]]
            [clojure.data.json :as json]
            [clj-time.format :as f]
            [clj-time.core :as t]))


(def custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY"))
(def db (mg/get-db (mg/connect) "athena"))
(def coll "tweets")
(def news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"])
(def data-node (atom nil))

(defn store
  "Stores the given tweet in mongodb"
  [tweet]
  (mc/insert db coll tweet))


;;TODO check if id exists in database
(defn read-data
  "Reads in json data from given path and stores it"
  [path]
  (doall (map #(store (json/read-str % :key-fn keyword)) (split (slurp path) #"\n"))))


(defn get-retweets
  "Fetches all retweets of given tweet id"
  [id]
  (->> (mc/find db coll {"retweeted_status.id" (Long/parseLong id)})
       seq
       (map #(from-db-object % true))))


(defn get-tweets
  "Fetches all tweets by a given twitter user"
  [user]
  (->> (mc/find db coll {"user.screen_name" user})
       seq
       (map #(from-db-object % true))
       (map #(update-in % [:created_at] (fn [x] (f/parse custom-formatter x))))))


(defn get-mentions
  "Fetches all tweets mentioning the given user"
  [user]
  (->> (mc/find db coll {"entities.user_mentions.screen_name" user})
       seq
       (map #(from-db-object % true))))


(defn get-all-retweets
  "Fetches all retweets of a given user"
  [user]
  (->> (mc/find db coll {"retweeted_status.user.screen_name" user})
       seq
       (map #(from-db-object % true))))


(defn get-replies
  "Fetches all replies to a given tweet id"
  [id]
  (->> (mc/find db coll {"in_reply_to_status_id" id})
       seq
       (map #(from-db-object % true))))


(comment

  (deref data-node)

  (swap! data-node (create-index news-accounts))

  (time (create-index news-accounts))

  (-> (mc/find db coll) seq first (from-db-object true))

)
