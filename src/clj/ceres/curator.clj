(ns ceres.curator
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.string :refer [split]]
            [net.cgrand.enlive-html :as enlive]
            [monger.conversion :refer [from-db-object]]
            [clojure.data.json :as json]
            [clj-time.format :as f]
            [taoensso.timbre :as timbre]
            [clj-time.core :as t])
 (:import org.bson.types.ObjectId))

(timbre/refer-timbre)

(def mongo-state
  (atom
   {:db (let [^MongoOptions opts (mg/mongo-options :threads-allowed-to-block-for-connection-multiplier 300)
              ^ServerAddress sa  (mg/server-address (or (System/getenv "DB_PORT_27017_TCP_ADDR") "127.0.0.1") 27017)]
          (mg/get-db (mg/connect sa opts) "athena"))
    :custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY")
    :news-account #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"}}))


(defn- expand-url
  "Expands shortened url strings, thanks to http://www.philippeadjiman.com/blog/2009/09/07/the-trick-to-write-a-fast-universal-java-url-expander/"
  [url-str]
  (let [url (java.net.URL. url-str)
        conn (.openConnection url)]
    (do (.setInstanceFollowRedirects conn false)
        (info "Expanding " url)
        (.connect conn)
        (let [expanded-url (.getHeaderField conn "Location")]
          (try
            (do (.close (.getInputStream conn))
                expanded-url)
            (catch Exception e (do (error (str e))
                                   (str "Not available"))))))))

(defn store-news
  "Stores news data"
  [record]
  (let [id (:_id record)
        news-source (-> record :user :screen_name)
        record-urls (-> record :entities :urls)
        url (if (empty? record-urls)
              nil
              (let [url-str (-> record-urls first :expanded_url)
                    expanded-url (expand-url url-str)]
                (if expanded-url
                  expanded-url
                  url-str)))
        oid (ObjectId.)]
    (info "Storing news " id)
    (mc/insert-and-return
     (:db @mongo-state)
     "news"
     {:_id oid
      :news-source news-source
      :url url
      :record-id id})))


(defn store
  "Stores the given tweet in mongodb"
  [tweet]
  (let [oid (ObjectId.)
        doc tweet
        record (mc/insert-and-return (:db @mongo-state) "tweets" (merge doc {:_id oid}))]
    (if ((:news-account @mongo-state) (-> tweet :user :screen_name))
      (store-news record)
      record)))


;;todo check if id exists in database
(defn read-data
  "Reads in json data from given path and stores it"
  [path]
  (doall (map #(store (json/read-str % :key-fn keyword)) (split (slurp path) #"\n"))))


(defn get-retweets
  "Fetches all retweets of given tweet id"
  [id]
  (->> (mc/find (:db @mongo-state) "tweets" {"retweeted_status.id" (Long/parseLong id)})
       seq
       (map #(from-db-object % true))))


(defn get-tweets
  "Fetches all tweets by a given twitter user"
  [user]
  (->> (mc/find (:db @mongo-state) "tweets" {"user.screen_name" user})
       seq
       (map #(from-db-object % true))
       (map #(update-in % [:created_at] (fn [x] (f/parse (:custom-formatter @mongo-state) x))))))


(defn get-mentions
  "Fetches all tweets mentioning the given user"
  [user]
  (->> (mc/find (:db @mongo-state) "tweets" {"entities.user_mentions.screen_name" user})
       seq
       (map #(from-db-object % true))))


(defn get-all-retweets
  "Fetches all retweets of a given user"
  [user]
  (->> (mc/find (:db @mongo-state) "tweets" {"retweeted_status.user.screen_name" user})
       seq
       (map #(from-db-object % true))))


(defn get-replies
  "Fetches all replies to a given tweet id"
  [id]
  (->> (mc/find (:db @mongo-state) "tweets" {"in_reply_to_status_id" id})
       seq
       (map #(from-db-object % true))))


(comment

  (->> (mc/find (:db @mongo-state) "tweets")
       seq
       (map #(from-db-object % true))
       (map #(-> % :user :screen_name))
       frequencies
       (map second)
       frequencies
       (sort-by first <))


  )