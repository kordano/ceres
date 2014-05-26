(ns ceres.warehouse
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [ceres.inspector :refer [levenshtein]]
            [clojure.string :refer [split]]
            [clojure.data.json :as json]
            [clj-time.format :as f]
            [clj-time.core :as t]))

(def custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY"))

(def db (mg/get-db (mg/connect) "athena"))

(def coll "tweets")

(defn read-data [path]
  (split (slurp path) #"\n"))

#_(time (doall (map #(insert (json/read-str % :key-fn keyword)) (read-data "/home/konny/data/tweets.json"))))

(defn insert [tweet]
  (mc/insert db coll tweet))


(defn inverted-index [news-accounts]
  (->> (mc/find db coll {"entities.user_mentions.screen_name" {$in news-accounts}})
       seq
       (map #(from-db-object % true))
       (remove #(nil? (:retweeted_status %)))
       (map #(vec [(-> % :retweeted_status :id) (:id %)]))
       (remove #(nil? (first %)))))


(defn find-related-tweets [tweet-id]
  (->> (mc/find db coll {"retweeted_status.id" (Long/parseLong tweet-id)})
       seq
       (map #(from-db-object % true))))


(defn get-news [news-account]
  (->> (mc/find db coll {"user.screen_name" news-account})
       seq
       (map #(from-db-object % true))
       (map #(update-in % [:created_at] (fn [x] (f/parse custom-formatter x))))))


(defn get-mentions[news-account]
  (->> (mc/find db coll {"entities.user_mentions.screen_name" news-account})
       seq
       (map #(from-db-object % true))))


(defn get-all-retweets [news-account]
  (->> (mc/find db coll {"retweeted_status.user.screen_name" news-account})
       seq
       (map #(from-db-object % true))))


(defn get-replies [id]
  (->> (mc/find db coll {"in_reply_to_status_id" id})
       seq
       (map #(from-db-object % true))))


(defn assemble [news-accounts]
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
       (into {})))

(comment

  (def news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"])

)
