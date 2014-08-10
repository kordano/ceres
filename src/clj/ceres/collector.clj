(ns ceres.collector
  (:refer-clojure :exclude [sort find])
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [monger.query :refer :all]
            [monger.joda-time]
            [net.cgrand.enlive-html :as enlive]
            [clj-time.format :as f]
            [taoensso.timbre :as timbre]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId))

(timbre/refer-timbre)

(def db (let [^MongoOptions opts (mg/mongo-options :threads-allowed-to-block-for-connection-multiplier 300)
              ^ServerAddress sa  (mg/server-address (or (System/getenv "DB_PORT_27017_TCP_ADDR") "127.0.0.1") 27017)]
          (mg/get-db (mg/connect sa opts) "athena")))

(def custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY"))

(def news-accounts #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ" "BILD" "DerWesten" "ntvde" "tazgezwitscher" "welt" "ZDFheute" "N24_de" "sternde" "focusonline"} )


(defn init-mongo []
  (do
    (mc/ensure-index db "articles" (array-map :ts 1))
    (mc/ensure-index db "origins" (array-map :ts 1))
    (mc/ensure-index db "origins" (array-map :source 1))
    (mc/ensure-index db "tweets" (array-map :user.screen_name 1))
    (mc/ensure-index db "tweets" (array-map :id_str 1))
    (mc/ensure-index db "tweets" (array-map :retweeted_status.id_str 1))
    (mc/ensure-index db "tweets" (array-map :in_reply_to_status_id_str 1))
    (mc/ensure-index db "tweets" (array-map :created_at 1))
    (mc/ensure-index db "tweets" (array-map :entities.user_mentions.screen_name 1 :retweeted_status.user.screen_name 1 :in_reply_to_screen_name 1))))


(defn- expand-url
  "Expands shortened url strings, thanks to http://www.philippeadjiman.com/blog/2009/09/07/the-trick-to-write-a-fast-universal-java-url-expander/"
  [url-str]
  (let [url (java.net.URL. url-str)
        conn (.openConnection url)]
    (do (.setInstanceFollowRedirects conn false)
        (.connect conn)
        (let [expanded-url (.getHeaderField conn "Location")
              content-type (.getContentType conn)]
          (try
            (do (.close (.getInputStream conn))
                {:url expanded-url
                 :content-type content-type})
            (catch Exception e (do (error (str e))
                                   {:url "Not available"
                                    :content-type content-type})))))))


(defn store-url [{:keys [article record ts source]}]
  (mc/insert-and-return
   db
   "urls"
   {:tweet record
    :article article
    :source source
    :ts ts}))


(defn store-origin [{:keys [article record ts source ancestors root]}]
  (mc/insert-and-return
   db
   "origins"
   {:tweet record
    :article article
    :source source
    :ts ts}))


(defn store-article [{:keys [url content-type ts] :as expanded-url}]
  (let [raw-html (slurp url)
        html-title (-> (java.io.StringReader. raw-html) enlive/html-resource (enlive/select [:head :title]) first :content first)]
    (mc/insert-and-return
     db
     "articles"
     {:url url
      :title html-title
      :content-type content-type
      :html raw-html
      :ts ts})))


(defn store
  "Stores the given tweet in mongodb"
  [tweet]
  (let [oid (ObjectId.)
        doc (update-in tweet [:created_at] (fn [x] (f/parse custom-formatter x)))
        record (from-db-object (mc/insert-and-return db "tweets" (merge doc {:_id oid})) true)
        ts (:created_at record)
        source (news-accounts (-> record :user :screen_name))
        record-urls (-> record :entities :urls)
        expanded-urls (if (empty? record-urls)
                        nil
                        (map #(let [expanded-url (expand-url (:expanded_url %))]
                                (if (:url expand-url)
                                  expanded-url
                                  (assoc expanded-url :url (:expanded_url %)))) record-urls))
        articles (if (nil? expanded-urls)
                   nil
                   (map #(assoc % :article (-> (mc/find-one-as-map db "articles" {:url (:url %)}) :_id)) expanded-urls))]
    (if (nil? articles)
      nil
      (doall
       (map
        #(if (:article %)
           (do (store-origin (assoc % :record oid :ts ts :source source))
               nil)
           (let [article (store-article (assoc % :ts ts))
                 origin (store-origin (assoc % :article (:_id article) :record oid :ts ts :source source))]
             {:article (update-in article [:ts] (fn [x] (f/unparse custom-formatter x))) :origin (str (:_id origin))}))
        articles)))))
