(ns ceres.warehouse
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [clj-time.format :as f]
            [clj-time.core :as t]))

(def custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY"))

(def db (mg/get-db (mg/connect) "athena"))

(def coll "tweets")

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


(comment

  (def news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"])

  (reduce + (map #(count (get-news %)) news-accounts))

  (->> (get-news "dpa")
       (sort-by :created_at t/after?))

  (->> (reduce (fn [old [k v]] (update-in old [k] (fn [x] (conj (or x #{}) v)))) {} (inverted-index news-accounts)) (map #(vec [(key %) (count (val %))]))
       (sort-by second >)
       (map second)
       frequencies
       (sort-by val >)
       time)

  (->> (mc/find db coll)
       seq
       last)

)
