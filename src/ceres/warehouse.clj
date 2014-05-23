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


(defn minkowski-distance [x y r]
  (Math/pow (reduce + (map #(Math/pow (Math/abs (- (first %) (second %))) r) (map vector x y))) (/ 1 r)))


(def users {"Angelica" {"Blues Traveler" 3.5
                        "Broken Bells" 2.0
                        "Norah Jones" 4.5
                        "Phoenix" 5.0
                        "Slightly Stoopid" 1.5
                        "The Strokes" 2.5
                        "Vampire Weekend" 2.0}
            "Bill" {"Blues Traveler" 2.0,
                    "Broken Bells" 3.5
                    "Deadmau5" 4.0
                    "Phoenix" 2.0
                    "Slightly Stoopid" 3.5
                    "Vampire Weekend" 3.0}
            "Chan" {"Blues Traveler" 5.0
                    "Broken Bells" 1.0
                    "Deadmau5" 1.0
                    "Norah Jones" 3.0
                    "Phoenix" 5
                    "Slightly Stoopid" 1.0}
            "Dan" {"Blues Traveler" 3.0
                   "Broken Bells" 4.0
                   "Deadmau5" 4.5
                   "Phoenix" 3.0
                   "Slightly Stoopid" 4.5
                   "The Strokes" 4.0
                   "Vampire Weekend" 2.0}
            "Hailey" {"Broken Bells" 4.0
                      "Deadmau5" 1.0
                      "Norah Jones" 4.0
                      "The Strokes" 4.0
                      "Vampire Weekend" 1.0}
            "Jordyn" {"Broken Bells" 4.5
                      "Deadmau5" 4.0
                      "Norah Jones" 5.0
                      "Phoenix" 5.0
                      "Slightly Stoopid" 4.5
                      "The Strokes" 4.0
                      "Vampire Weekend" 4.0}
            "Sam" {"Blues Traveler" 5.0
                   "Broken Bells" 2.0
                   "Norah Jones" 3.0
                   "Phoenix" 5.0
                   "Slightly Stoopid" 4.0
                   "The Strokes" 5.0}
            "Veronica" {"Blues Traveler" 3.0
                        "Norah Jones" 5.0
                        "Phoenix" 4.0
                        "Slightly Stoopid" 2.5
                        "The Strokes" 3.0}})

(let [user-1 (vals (users "Hailey"))]
  (minkowski-distance [1 2 3] [2 3 4] 2))

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

)
