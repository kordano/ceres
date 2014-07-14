(ns ceres.curator
  (:refer-clojure :exclude [sort find])
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [monger.query :refer :all]
            [monger.joda-time]
            [clojure.string :refer [split join]]
            [net.cgrand.enlive-html :as enlive]
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
    :news-accounts #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ" "BILD" "DerWesten" "ntvde" "tazgezwitscher" "welt" "ZDFheute" "N24_de" "sternde" "focusonline"}}))

(def months
  [(range 1 32)
   (range 1 29)
   (range 1 32)
   (range 1 31)
   (range 1 32)
   (range 1 31)
   (range 1 32)
   (range 1 32)
   (range 1 31)
   (range 1 32)
   (range 1 31)
   (range 1 32)])

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

(defn store-url [record]
  (let [id (:_id record)
        record-urls (-> record :entities :urls)
        urls (if (empty? record-urls)
               nil
               (map
                #(let [expanded-url (expand-url (:expanded_url %))]
                   (if expanded-url
                     expanded-url
                     (:expanded_url %)))
                record-urls))]
    (if urls
      (doall
       (pmap
        #(mc/insert-and-return
         (:db @mongo-state)
         "urls"
         {:record-id id
          :url %})
        urls)))))

(defn store
  "Stores the given tweet in mongodb"
  [tweet]
  (let [oid (ObjectId.)
        doc (update-in tweet [:created_at] (fn [x] (f/parse (:custom-formatter @mongo-state) x)))
        record (mc/insert-and-return (:db @mongo-state) "tweets" (merge doc {:_id oid}))]
    (if ((:news-accounts @mongo-state) (-> record :user :screen_name))
      (store-news record)
      record)
    (store-url record)))


;;todo check if id exists in database
(defn read-data
  "Reads in json data from given path and stores it"
  [path]
  (doall (map #(let [data (json/read-str % :key-fn keyword)]
                 (println "Importing " (:id data))
                 (store data)) (split (slurp path) #"\n"))))


(defn export-edn [m d]
  (->> (get-tweets-from-date m d)
       (map #(dissoc % :_id))
       (map #(update-in % [:created_at] (fn [x] (f/unparse (:custom-formatter @mongo-state) x))))
       (map str)
       (clojure.string/join "\n")))


(defn get-retweets
  "Fetches all retweets of given tweet id"
  [id]
  (->> (mc/find (:db @mongo-state) "tweets" {"retweeted_status.id" (Long/parseLong id)})
       seq
       (map #(from-db-object % true))))


(defn get-tweets
  "Fetches all tweets by a given twitter user"
  [user]
  (->> (mc/find (:db @mongo-state) "tweets" {:user.screen_name user :created_at {$gt (t/date-time 2014 7 1)}})
       seq
       (map #(from-db-object % true))))


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


(defn get-recent-tweets
  "Retrieve the last 25*n tweets"
  [n]
  (->> (mc/find (:db @mongo-state) "tweets")
       seq
       (take-last (+ (* n 25) 100))
       (take 25)
       (mapv #(from-db-object % true))))


(defn get-news-frequencies []
  (mapv #(vec [% (mc/count (:db @mongo-state) "tweets" {:user.screen_name %
                                                        :created_at {$gt (t/date-time 2014 7 1)}})]) (:news-accounts @mongo-state)))


(defn get-tweet-count []
  (mc/count (:db @mongo-state) "tweets" {:created_at {$gt (t/date-time 2014 7 1)}}))


(defn get-tweets-from-date [month day]
  (->> (mc/find
        (:db @mongo-state)
        "tweets"
        {:created_at
         {$gt (t/date-time 2014 month day 0 0 0 0)
          $lte (t/date-time 2014 month day 23 59 59 999)}
         })
       seq
       (pmap #(from-db-object % true))))


(defn get-hashtag-frequencies [coll]
  (->> coll
       (map #(from-db-object % true))
       (map #(map (fn [hashtag] (hashtag :text)) (-> % :entities :hashtags)))
       flatten
       frequencies))


(defn compute-diffusion [user]
  (->> (mc/count (:db @mongo-state) "tweets" {$and [{$or [{"entities.user_mentions.screen_name" user}
                                                           {"retweeted_status.user.screen_name" user}
                                                           {"in_reply_to_screen_name" user}]}
                                                    {:created_at {$gt (t/date-time 2014 7 1)}}]})))

(defn compute-tweet-diffusion [tweet-id]
  (let [neighbor-tweets (->> (mc/find
                              (:db @mongo-state)
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]})
                             (pmap #(from-db-object % true)))
        neightbar-ids (pmap :id_str neighbor-tweets)]
    (merge neighbor-tweets (pmap #(compute-tweet-diffusion %) neightbar-ids))))


(defn get-news-diffusion []
  (mapv #(vec [% (compute-diffusion %)]) (:news-accounts @mongo-state)))


(defn get-month-distribution [month]
  (let [day-range (months (dec month))]
    (vec
     (pmap
      (fn [day]
        (into {} [[:date (str (t/date-time 2014 month day))]
                  [:count
                   (mc/count
                    (:db @mongo-state)
                    "tweets"
                    {:created_at
                     {$gt (t/date-time 2014 month day 0 0 0 0)
                      $lte (t/date-time 2014 month day 23 59 59 999)}})]]))
      day-range))))


(comment

  (->> (mc/find (:db @mongo-state) "urls")
       (map #(from-db-object % true))
       (map :url)
       frequencies)


  (->>
   (mc/find
    (:db @mongo-state)
    "tweets"
    {:created_at
     {$gt (t/date-time 2014 6 16 0 0 0 0)
      $lte (t/date-time 2014 6 16 23 59 59 999)}})
   seq
   (map #(from-db-object % true))
   (map (fn [tweet] (map (fn [hashtag] (:text hashtag)) (get-in tweet [:entities :hashtags]))))
   flatten
   frequencies
   (sort-by val >)
   (take 10))



  ;; TODO update on server
  (time
   (doseq [x (monger.collection/find-maps (:db @mongo-state) "tweets")]
     (mc/update-by-id
      (:db @mongo-state)
      "tweets"
      (:_id x)
      (update-in x [:created_at] #(f/parse (:custom-formatter @mongo-state) (:created_at %))))))


  (get-tweet-count)

  (->> (mc/find (:db @mongo-state) "tweets")
       seq
       (map #(dissoc :_id (from-db-object % true)))
       count)




)
