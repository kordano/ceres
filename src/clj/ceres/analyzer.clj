(ns ceres.analyzer
  (:refer-clojure :exclude [sort find])
  (:require [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.query :refer :all]
            [clojure.data.json :as json]
            [monger.joda-time]
            [clojure.string :refer [split join lower-case]]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [aprint.core :refer [aprint]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clj-time.periodic :as p]
            [clojure.pprint :refer [pprint]]
            [opennlp.nlp :refer [make-tokenizer make-detokenizer]]
            [incanter.stats :refer [mean variance quantile]]

            [loom.graph :as lg]
            [loom.io :as lio]
            [ceres.collector :refer [db custom-formatter news-accounts store]])
 (:import org.bson.types.ObjectId))

(def time-interval {$gt (t/date-time 2014 8 1) $lt (t/date-time 2014 9 1)})

(defrecord Publication [source reactions])

(defn short-metrics [coll]
  {:mean (mean coll)
   :std (Math/sqrt (variance coll))
   :quantiles (quantile coll)})


(defn find-reactions [pid]
  (let [reactions (mc/find-maps @db "reactions" {:source pid})]
    (Publication. pid (vec (pmap #(find-reactions (:publication %)) reactions)))))


(defn reaction-tree [pub]
  (zip/zipper
   (fn [node] true)
   (fn [node] (:reactions node))
   (fn [node new-children] (assoc-in node [:reactions] new-children))
   (find-reactions pub)))


(defn find-full-reactions
  "extended reaction tree recursion"
  [pid]
  (let [publication (mc/find-map-by-id @db "publications" pid)
        reactions (mc/find-maps @db "reactions" {:source pid})]
    (Publication. publication (vec (pmap #(find-full-reactions (:publication %)) reactions)))))


(defn full-reaction-tree [pub]
  (zip/zipper
   (fn [node] true)
   (fn [node] (:reactions node))
   (fn [node new-children] (assoc-in node [:reactions] new-children))
   (find-reactions pub)))


(defn summary [tree]
  (loop [size 0
         max-path 0
         loc tree]
    (if (zip/end? loc)
      {:size size
       :height max-path}
      (recur
       (if (zip/node loc) (inc size) size)
       (if (zip/node loc) (-> loc zip/path count (max max-path)) max-path)
       (zip/next loc)))))


(defn hashtags-of-the-day [date]
  (let [pubs (mc/find-maps @db "publications" {:ts {$gt date
                                                    $lt (t/plus date (t/days 1))}})]
    (->> pubs
         (map :hashtags)
         flatten
         (remove nil?)
         (pmap #(mc/find-map-by-id @db "hashtags" %))
         (pmap :text)
         frequencies
         (sort-by second >)
         (take 25))))


(defn users-of-the-day
  "Get user with most posts of given date"
  [date]
  (let [pubs (mc/find-maps @db "publications" {:ts {$gt date
                                                    $lt (t/plus date (t/days 1))}})]
    [(count pubs)
     (->> pubs
          (map :user)
          frequencies
          (sort-by second >)
          (take 25)
          (map (fn [[k v]] [(:screen_name (mc/find-map-by-id @db "users" k))
                           v])))]))


(comment


  ;; hashtag distribution of one-time-posters
  (let [user-freq (->> (mc/find-maps @db "publications")
                       (map :user)
                       frequencies)]
    (->> user-freq
         (remove (fn [[k v]] (> v 1)))
         keys
         (map #(mc/find-one-as-map @db "publications" {:user %}))
         (map :hashtags)
         flatten
         (remove nil?)
         frequencies
         (sort-by second >)
         (take 25)
         (map (fn [[k v]] [(:text (mc/find-map-by-id @db "hashtags" k))
                          v]))
         aprint
         time))


  ;; hashtag distribution
  (->> (mc/find-maps @db "publications")
       (map :hashtags)
       (remove nil?)
       flatten
       frequencies
       (sort-by second >)
       (take 25)
       (map (fn [[k v]] [(:text (mc/find-map-by-id @db "hashtags" k))
                        v]))
       aprint
       time)


  (let [users (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}}))]
    (->> (mc/find-maps @db "publications" {:user {$in users}})
         rand-nth
         :_id
         full-reaction-tree
         aprint
         time))

  (def source-uids (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}})))

  (def source-publications (mc/find-maps @db "publications" {:user {$in source-uids}}))

  (def user-publications (mc/find-maps @db "publications" {:user {$nin source-uids}}))


  ;; pub counts
  (let [user-pub-count (mc/count @db "publications" {:user {$nin source-uids}})
      source-pub-count (mc/count @db "publications" {:user {$in source-uids}})
      overall-pub-count (mc/count @db "publications")]
    (aprint [((comp float /) user-pub-count overall-pub-count)
             ((comp float /) source-pub-count overall-pub-count)]))

  ;; user counts
  (let [user-count (mc/count @db "users" {:screen_name {$nin news-accounts}})
      source-count (mc/count @db "users" {:screen_name {$in news-accounts}})
      overall-count (mc/count @db "users")]
    (aprint [(Math/log10 ((comp float /) user-count overall-count))
             (Math/log10 ((comp float /) source-count overall-count))]))


  ;; avg pub per source
  (->> (map (fn [uid] [uid (mc/count @db "publications" {:user uid})]) source-uids)
       (map second)
       short-metrics
       aprint)


  ;; avg pub per user
  (->> (mc/find-maps @db "publications" {:user {$nin source-uids}})
       (map :user)
       frequencies
       vals
       short-metrics
       aprint
       time)

  )
