(ns ceres.collector
  (:refer-clojure :exclude [sort find])
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [monger.conversion :refer [from-db-object]]
            [monger.query :refer :all]
            [monger.joda-time]
            [aprint.core :refer [aprint]]
            [net.cgrand.enlive-html :as enlive]
            [clj-time.format :as f]
            [speech-synthesis.say :as say]
            [taoensso.timbre :as timbre]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId))


(timbre/refer-timbre)


(def db (atom
         (let [^MongoOptions opts (mg/mongo-options :threads-allowed-to-block-for-connection-multiplier 300)
               ^ServerAddress sa  (mg/server-address (or (System/getenv "DB_PORT_27017_TCP_ADDR") "127.0.0.1") 27017)]
           (mg/get-db (mg/connect sa opts) "athena"))))


(def time-interval {$gt (t/date-time 2014 8 1) $lt (t/date-time 2014 9 1)})


(defn set-db [name]
  (let [^MongoOptions opts (mg/mongo-options :threads-allowed-to-block-for-connection-multiplier 300)
        ^ServerAddress sa  (mg/server-address (or (System/getenv "DB_PORT_27017_TCP_ADDR") "127.0.0.1") 27017)]
    (reset! db (mg/get-db (mg/connect sa opts) name))))


(def custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY"))


(def news-accounts #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ" "BILD" "DerWesten" "ntvde" "tazgezwitscher" "welt" "ZDFheute" "N24_de" "sternde" "focusonline"} )


(defn init-mongo
  "Define mongodb indices on first start"
  []
  (do
    (mc/ensure-index @db "articles" (array-map :ts 1))
    (mc/ensure-index @db "origins" (array-map :ts 1))
    (mc/ensure-index @db "origins" (array-map :source 1))
    (mc/ensure-index @db "origins" (array-map :tweet 1))
    (mc/ensure-index @db "origins" (array-map :article 1 :source 1))
    (mc/ensure-index @db "publications" (array-map :tweet 1))
    (mc/ensure-index @db "publications" (array-map :user 1))
    (mc/ensure-index @db "publications" (array-map :ts 1))
    (mc/ensure-index @db "publications" (array-map :url 1))
    (mc/ensure-index @db "reactions" (array-map :source 1))
    (mc/ensure-index @db "reactions" (array-map :publication 1))
    (mc/ensure-index @db "urls" (array-map :url 1))
    (mc/ensure-index @db "hashtags" (array-map :text 1))
    (mc/ensure-index @db "mentions" (array-map :user 1))
    (mc/ensure-index @db "mentions" (array-map :publication 1))
    (mc/ensure-index @db "users" (array-map :id 1))
    (mc/ensure-index @db "tweets" (array-map :user.screen_name 1))
    (mc/ensure-index @db "tweets" (array-map :id_str 1))
    (mc/ensure-index @db "tweets" (array-map :id 1))
    (mc/ensure-index @db "tweets" (array-map :retweeted_status.id_str 1))
    (mc/ensure-index @db "tweets" (array-map :in_reply_to_status_id_str 1))
    (mc/ensure-index @db "tweets" (array-map :retweeted_status.id 1))
    (mc/ensure-index @db "tweets" (array-map :in_reply_to_status_id 1))
    (mc/ensure-index @db "tweets" (array-map :in_reply_to_user_id_str 1))
    (mc/ensure-index @db "tweets" (array-map :created_at 1))
    (mc/ensure-index @db "tweets" (array-map :entities.user_mentions.screen_name 1 :retweeted_status.user.screen_name 1 :in_reply_to_screen_name 1))))


(defn expand-url
  "Expands shortened url strings, thanks to http://www.philippeadjiman.com/blog/2009/09/07/the-trick-to-write-a-fast-universal-java-url-expander/"
  [url-str]
  (let [url (java.net.URL. url-str)
        conn (try (.openConnection url)
                  (catch Exception e (do (error (str e))
                                         false)))]
    (if conn
      (do (.setInstanceFollowRedirects conn false)
          (try
            (do
              (.connect conn)
              (let [expanded-url (.getHeaderField conn "Location")
                    content-type (.getContentType conn)]
                (try
                  (do (.close (.getInputStream conn))
                      {:url expanded-url
                       :content-type content-type})
                  (catch Exception e (do (error (str e))
                                         {:url :not-available
                                          :content-type content-type})))))
            (catch Exception e (do (error (str e))
                                   nil))))
      nil)))


(defn store-origin
  "Store relationship between article and tweet"
  [{:keys [article record ts source]}]
  (mc/insert-and-return
   @db
   "origins"
   {:tweet record
    :article article
    :source source
    :ts ts}))


(defn fetch-url [url]
  (try
    (enlive/html-resource (java.net.URL. url))
    (catch Exception e :error)))


(defn fetch-url-title
  "fetch url and extract title"
  [url]
  (let [res (fetch-url url)]
    (if (= :error res)
      url
      (-> res (enlive/select [:head :title]) first :content first))))


(defn store-article
  "Fetch html document, extract title and store them"
  [{:keys [url content-type ts] :as expanded-url}]
  (let [raw-html (if (= url "Not available") nil (slurp url))
        html-title (if raw-html
                     (-> (java.io.StringReader. raw-html)
                         enlive/html-resource
                         (enlive/select [:head :title])
                         first
                         :content
                         first)
                     nil)
        #_(fetch-url-title url)]
    (mc/insert-and-return
     @db
     "articles"
     {:url url
      :title html-title
      :content-type content-type
      :html raw-html
      :ts ts})))


(defn store
  "Store the tweet"
  [tweet]
  (let [oid (ObjectId.)
        doc (update-in tweet [:created_at] (fn [x] (f/parse custom-formatter x)))
        record (from-db-object (mc/insert-and-return @db "tweets" (merge doc {:_id oid})) true)
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
                   (map #(assoc % :article (-> (mc/find-one-as-map @db "articles" {:url (:url %)}) :_id)) expanded-urls))]
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


(defn store-user [{{:keys [id screen_name followers_count created_at]} :user}]
  (let [date (f/parse custom-formatter created_at)]
      (mc/insert-and-return
       @db
       "users"
       {:id id
        :screen_name screen_name
        :created_at date})))


(defn store-followers-count [uid followers-count date]
  (mc/insert
   @db
   "followers"
   {:user uid
    :followers-count followers-count
    :date date}))


(defn store-publication [uid tid url-id type hids ts]
  (mc/insert-and-return
   @db
   "publications"
   {:user uid
    :tweet tid
    :url url-id
    :type type
    :hashtags hids
    :ts ts}))


(defn store-url [url uid tid ts]
  (mc/insert
   @db
   "urls"
   {:url url
    :user uid
    :tweet tid
    :ts ts}))


(defn store-tmp-url [url uid tid ts]
  (mc/insert
   @db
   "tmpurls"
   {:url url
    :user uid
    :tweet tid
    :ts ts}))


(defn store-mention
  [uid pid]
  (mc/insert
   @db
   "mentions"
   {:user uid
    :publication pid}))


(defn store-hashtag [text]
  (mc/insert-and-return
   @db
   "hashtags"
   {:text text}))


(defn store-reaction
  [pub-id source-id]
  (mc/insert @db "reactions" {:publication pub-id :source source-id}))


(defn get-user-id [{:keys [user] :as status}]
  (if-let [uid (:_id (mc/find-one-as-map @db "users" {:id (:id user)}))]
    uid
    (:_id (store-user status))))


(defn get-hashtag-id [text]
  (if-let [hid (:_id (mc/find-one-as-map @db "hashtags" {:text text}))]
    hid
    (:_id (store-hashtag text))))


(defn store-raw-html
  "Fetch html document and store raw binary in database"
  [{:keys [url content-type ts] :as expanded-url} url-id]
  (let [raw-html (if (= url :not-available)
                   nil
                   (slurp url))
        html-title (if raw-html
                     (-> (java.io.StringReader. raw-html)
                         enlive/html-resource
                         (enlive/select [:head :title])
                         first
                         :content
                         first)
                     :not-available)]
    (mc/insert-and-return
     @db
     "htmls"
     {:raw raw-html
      :url url-id})))


(defn get-url-id
  "Get url id if exists otherwise store url"
  [url uid tid ts source?]
  (if-let [url-id (:_id (mc/find-one-as-map @db "urls" {:url url}))]
    url-id
    (if-let [expanded-url (expand-url url)]
      (if-let [x-url-id (:_id (mc/find-one-as-map @db "urls" {:url (:url expanded-url)}))]
        x-url-id
        (if source?
          (let [new-url-id (:_id (store-url (:url expanded-url) uid tid ts))]
            (do
              (store-raw-html expanded-url new-url-id)
              new-url-id))
          nil))
      nil)))


(defn get-type
  "Dispatches tweet type"
  [{:keys [in_reply_to_status_id retweeted_status entities]}]
  (if in_reply_to_status_id
    :reply
    (if retweeted_status
      :retweet
      (if-not (empty? (:urls entities))
        :source-or-share
        :unrelated))))


(defn store-simple-reaction
  "Store tweet as publication and reaction"
  [uid tid type hids ts source-id]
  (let [source-tid (:_id (mc/find-one-as-map @db "tweets" {:id source-id}))
        source-pub-id (if source-tid
                        (:_id (mc/find-one-as-map @db "publications" {:tweet source-tid}))
                        nil)
        pub-id (:_id (do (store-publication uid tid nil type hids ts)))]
    (when source-pub-id
      (store-reaction pub-id source-pub-id))))


(defn store-raw-tweet
  "Basic status pipeline"
  [status]
  (let [oid (ObjectId.)
        doc (update-in status [:created_at] (fn [x] (f/parse custom-formatter x)))
        {:keys [user entities retweeted_status in_reply_to_status_id created_at _id]
         :as record} (from-db-object (mc/insert-and-return @db "tweets" (merge doc {:_id oid})) true)
        uid (get-user-id status)
        hids (doall (map (fn [{:keys [text]}] (get-hashtag-id text)) (:hashtags entities)))
        type (get-type status)
        source? (news-accounts (:screen_name user))]
    (case type
      :retweet (do (store-simple-reaction uid _id :retweet hids created_at (:id retweeted_status)))
      :reply (do (store-simple-reaction uid _id :reply hids created_at in_reply_to_status_id))
      :source-or-share (let [url-ids (doall (map
                                             #(get-url-id (:expanded_url %) uid _id created_at source?)
                                             (:urls entities)))]
                         (if source?
                           (store-publication uid _id (first url-ids) :source hids created_at)
                           (let [source-pub-id (or (doall (map #(:_id (mc/find-one-as-map @db "publications" {:url %})) url-ids)))
                                 pub-id (:_id (store-publication uid _id nil :share hids created_at))]
                             (when source-pub-id
                               (store-reaction pub-id source-pub-id)))))
      :unrelated (store-publication uid _id nil :unrelated hids created_at))
    record))
