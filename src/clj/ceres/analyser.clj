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
            [clojure.java.shell :refer [sh]]
            [opennlp.nlp :refer [make-tokenizer make-detokenizer]]
            [loom.graph :as lg]
            [loom.io :as lio]
            [ceres.collector :refer [db custom-formatter news-accounts store]])
 (:import org.bson.types.ObjectId))

(def time-interval {$gt (t/date-time 2014 8 1) $lt (t/date-time 2014 9 1)})

(defrecord Publication [source reactions])

(defn find-reactions [pid]
  (let [reactions (mc/find-maps @db "reactions" {:source pid})]
    (Publication. pid (vec (pmap #(find-reactions (:publication %)) reactions)))))


(defn reaction-tree [pub]
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



(comment {}

  ;; reaction to all publications
  (let [users (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}}))]
    (->> (mc/find-maps @db "published" {:user {$in users}})
         (pmap #(find-reactions (:_id %)))
         first
         reaction-tree
         aprint
         time))


  )
