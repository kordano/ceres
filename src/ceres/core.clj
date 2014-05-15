(ns ceres.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [clj-time.core :as t]))




(defn inverted-index [db coll news-accounts acc]
  (let [news-tweets (->> (mc/find db coll {"user.screen_name" {$in news-accounts}})
                          seq
                          (map #(from-db-object % true))
                          (map #(vec [(:id %) %]))
                          (into {}))]
      (->> (mc/find db coll {"entities.user_mentions.screen_name" {$in news-accounts}})
           seq
           (map #(from-db-object % true))
           (remove #(nil? (:retweeted_status %)))
           (map #(news-tweets (-> % :retweeted_status :id)))
           (remove nil?)
           (frequencies)
           (map #(into {} [[:text (-> % key :text)] [:count (val %)] [:user (-> % key :user :screen_name)]]))
           (sort-by :count >)
           (filter #(= (:user %) acc)))))


#_(let [conn (mg/connect)
      db (mg/get-db conn "athena")
      coll "tweets"
      news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"]]
    (count (inverted-index db coll news-accounts "SZ")))
