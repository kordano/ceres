(ns ceres.warehouse
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [clj-time.core :as t]))


(defn inverted-index [news-accounts]
  (let [conn (mg/connect)
        db (mg/get-db conn "athena")
        coll "tweets"
        news-tweets (->> (mc/find db coll {"user.screen_name" {$in news-accounts}})
                          seq
                          (map #(from-db-object % true))
                          (map #(vec [(:id %) %]))
                          (into {}))]
      (->> (mc/find db coll {"entities.user_mentions.screen_name" {$in news-accounts}})
           seq
           (map #(from-db-object % true))
           (remove #(nil? (:retweeted_status %)))
           (map #(vec [(-> % :retweeted_status :id) (:id %)]))
           (remove #(nil? (first %))))))


(defn find-related-tweets [tweet-id]
  (let [conn (mg/connect)
        db (mg/get-db conn "athena")
        coll "tweets"]
    (->> (mc/find db coll {"retweeted_status.id" (Long/parseLong tweet-id)})
         seq
         (map #(from-db-object % true)))))


(defn get-news [news-account]
  (let [conn (mg/connect)
        db (mg/get-db conn "athena")
        coll "tweets"]
    (->> (mc/find db coll {"user.screen_name" news-account})
         seq
         (map #(from-db-object % true))
         )))


(defn get-news-tweets [news-accounts]
  (let [conn (mg/connect)
      db (mg/get-db conn "athena")
        coll "tweets"]
    (->> (mc/find db coll {"user.screen_name" {$in news-accounts}})
         seq
         (map #(from-db-object % true))
         (map #(vec [(:id %) %]))
         (into {}))))


(defn get-news-counts [news-accounts]
 (let [conn (mg/connect)
      db (mg/get-db conn "athena")
      coll "tweets"]
    (into {}
          (map
           #(->> (vec [% (-> (mc/find db coll {"user.screen_name" %})
                             seq
                             count)]))
           news-accounts))) )

(comment

  (def news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"])

  (->> (get-news-tweets news-accounts) ffirst)

  (let [conn (mg/connect)
        db (mg/get-db conn "athena")
        coll "tweets"
        id (->> (reduce (fn [old [k v]] (update-in old [k] #(conj (or %1 #{}) v))) {} (inverted-index news-accounts))
                (map #(vec [(key %) (count (val %))]) )
                vec
                (sort-by second > )
                (take 5 )
                last
                first)]
    (-> (first (seq (mc/find db coll {"id" id})))
        (from-db-object true)
        :text))




  )
