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
        conn (try
               (.openConnection url)
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
                                         {:url "Not available"
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
  (let [raw-html url #_(if (= url "Not available") nil (slurp url))
        html-title (fetch-url-title url) #_(if raw-html (-> (java.io.StringReader. raw-html) enlive/html-resource (enlive/select [:head :title]) first :content first)
                       nil)]
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
  "Yeah"
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
  "a reaction"
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

(defn get-url-id
  "Get url id if exists otherwise store url"
  [url uid tid ts]
  (if-let [url-id (:_id (mc/find-one-as-map @db "urls" {:url url}))]
    url-id
    (if-let [expanded-url (:url (expand-url url))]
      (if-let [x-url-id (:_id (mc/find-one-as-map @db "urls" {:url expanded-url}))]
        x-url-id
        (:_id (store-url expanded-url uid tid ts)))
      nil)))


(defn get-type [{:keys [in_reply_to_status_id retweeted_status entities]}]
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


(defn store-raw-tweet [status]
  (let [oid (ObjectId.)
        doc (update-in status [:created_at] (fn [x] (f/parse custom-formatter x)))
        {:keys [user entities retweeted_status in_reply_to_status_id created_at _id]
         :as record} (from-db-object (mc/insert-and-return @db "tweets" (merge doc {:_id oid})) true)
        uid (get-user-id status)
        hids (doall (map (fn [{:keys [text]}] (get-hashtag-id text)) (:hashtags entities)))
        type (get-type status)]
    (case type
      :retweet (do (store-simple-reaction uid _id :retweet hids created_at (:id retweeted_status)))
      :reply (do (store-simple-reaction uid _id :reply hids created_at in_reply_to_status_id))
      :source-or-share (let [url-ids (doall (map #(get-url-id (:expanded_url %) uid _id created_at) (:urls entities)))]
                         (if (news-accounts (:screen_name user))
                           (store-publication uid _id (first url-ids) :source hids created_at)
                           (let [source-pub-id (or (doall (map #(:_id (mc/find-one-as-map @db "publications" {:url %})) url-ids)))
                                 pub-id (:_id (store-publication uid _id nil :share hids created_at))]
                             (when source-pub-id
                               (store-reaction pub-id source-pub-id)))))
      :unrelated (store-publication uid _id nil :unrelated hids created_at))))


(comment

  ;; export users
  (time
   (letfn [(user-exists? [{{:keys [id]} :user}] (mc/find-one @db "users" {:id id}))]
     (let [tweets (mc/find-maps @db "tweets" {:created_at {$gt (t/date-time 2014 8 1)
                                                           $lt (t/date-time 2014 9 1)}})]
       (doall
        (for [tweet tweets]
          (when-not (user-exists? tweet)
            (store-user tweet)))))))




  ;; export urls
  (time
   (do
     (println "Export-urls")
     (let [origins (mc/find-maps @db "origins" {:source {$ne nil}
                                                :ts {$gt (t/date-time 2014 8 1)
                                                     $lt (t/date-time 2014 9 1)}})]
       (doall
        (map
         (fn [{:keys [article tweet ts source]}]
           (let [uid (:_id (mc/find-one-as-map @db "users" {:screen_name source}))
                 url (if article
                       (:url (mc/find-map-by-id @db "articles" article))
                       nil)]
             (if url
               (store-url url uid tweet ts)
               :unknown)))
         origins)))))


  ;; export un-parsed urls
  (letfn [(expand-urls [{{:keys [urls]} :entities}]
            (pmap :expanded_url urls))]
    (let [tweets (mc/find-maps @db "tweets" {:created_at {$gt (t/date-time 2014 7 1)
                                                          $lt (t/date-time 2014 7 29)}
                                             :entities.urls {$ne []}
                                             :user.screen_name {$in news-accounts}})]
      (time
       (doall
        (pmap
         (fn [tweet]
           (let [urls (expand-urls tweet)
                 uid (:_id (mc/find-one-as-map @db "users" {:screen_name (-> tweet :user :screen_name)}))]
             (pmap
              #(store-tmp-url % uid (:_id tweet) (:created_at tweet))
              urls)))
         tweets)))))


  (letfn [(get-url-id [{{urls :urls} :entities}]
            (if-not (empty? urls)
              (:_id (mc/find-one-as-map @db "url" {:url (-> urls first :url expand-url :url)}))
              nil))
          (get-uid [{{id :id} :user}]
            (:_id (mc/find-one-as-map @db "users" {:id id})))
          (dispatch-tweet [{:keys [retweeted_status in_reply_to_status_id]}]
            (if in_reply_to_status_id
              :reply
              (if retweeted_status
                :retweet
                nil)))
          (get-type [tweet url-id]
            (if-let [ttype (dispatch-tweet tweet)]
              ttype
              (if url-id
                (if (news-accounts (-> tweet :user :screen_name))
                  :source
                  :share)
                :unknown)))]
    (let [tweets (mc/find-maps @db "tweets" {:created_at {$gt (t/date-time 2014 7 1)
                                                          $lt (t/date-time 2014 7 7)}
                                             :user.screen_name {$in news-accounts}
                                             :retweeted_status.id_str nil
                                             :in_reply_to_user_id nil
                                             :entities.urls {$ne []}})]
      (time
       (aprint
        (->> tweets
             (take 10)
             (pmap get-url-id)
             #_(pmap #(let [url-id (get-url-id %)]
                        (store-publication (get-uid %)
                                         (:_id %)
                                         url-id
                                         (get-type % url-id)
                                         (:created_at %)))))))))



  ;; export tmp urls
  (letfn [(x-url [url]
            (let [expanded-url (-> url :url expand-url :url)]
              (if expanded-url
                expanded-url
                url)))]
    (time
     (do
       (println "Export tmp urls")
       (doall
        (->> (mc/find-maps @db "tmpurls")
             (pmap (fn [{:keys [user tweet ts] :as url}]
                     (store-url (x-url url) user tweet ts))))))))


  ;; export all hashtags
  (letfn [(extract-hashtags [{{:keys [hashtags]} :entities}]
            (pmap :text hashtags))]
    (let [tweets (mc/find-maps @db "tweets" {:created_at {$gt (t/date-time 2014 8 1)
                                                          $lt (t/date-time 2014 9 1)}
                                             :entities.hashtags {$ne []}})]
      (time
       (doall
        (->> tweets
             (pmap extract-hashtags)
             flatten
             (remove nil?)
             (into #{})
             (pmap store-hashtag))))))

  ;; export news publications
  (letfn [(dispatch-type [{:keys [in_reply_to_status_id retweeted_status entities]}]
            (if in_reply_to_status_id
              :reply
              (if retweeted_status
                :retweet
                (if-not (empty? (:urls entities))
                  :source-or-share
                  :unrelated))))
          (transact-publications [{:keys [user _id entities created_at] :as tweet}]
            (let [uid (:_id (mc/find-one-as-map @db "users" {:id (:id user)}))
                  url-id (if-not (empty? (:urls entities))
                           (:_id (mc/find-one-as-map @db "urls" (:tweet _id)))
                           nil)
                  hids (if-not (empty? (:hashtags entities))
                         (->> (:hashtags entities)
                              (pmap #(:_id (mc/find-one-as-map @db "hashtags" {:text (:text %)})))
                              (into #{})
                              vec)
                         nil)
                  t-type (dispatch-type tweet)]
              (store-publication uid _id url-id t-type hids created_at)))]
    (let [tweets (mc/find-maps @db "tweets"
                               {:created_at {$gt (t/date-time 2014 8 1)
                                             $lt (t/date-time 2014 9 1)}
                                :user.screen_name {$in news-accounts}})]
      (time
       (doall
        (pmap transact-publications tweets)))))

  ;; export retweets
  (letfn [(transact-publications [{:keys [user _id created_at entities id_str retweeted_status] :as tweet}]
            (let [uid (:_id (mc/find-one-as-map @db "users" {:id (:id user)}))
                  hids (if-not (empty? (:hashtags entities))
                         (->> (:hashtags entities)
                              (pmap #(:_id (mc/find-one-as-map @db "hashtags" {:text (:text %)})))
                              (into #{})
                              vec)
                         nil)]
              (store-publication uid _id nil :retweet hids created_at)))]
    (let [retweets (mc/find-maps @db "tweets" {:created_at {$gt (t/date-time 2014 8 1)
                                                            $lt (t/date-time 2014 9 1)}
                                               :retweeted_status {$ne nil}
                                               :user.screen_name {$nin news-accounts}})]
      (time (doall (pmap transact-publications retweets)))))



  ;; export replies
  (letfn [(transact-publications [{:keys [user _id created_at entities id_str in_reply_to_status_id_str] :as tweet}]
            (let [uid (:_id (mc/find-one-as-map @db "users" {:id (:id user)}))
                  hids (if-not (empty? (:hashtags entities))
                         (->> (:hashtags entities)
                              (pmap #(:_id (mc/find-one-as-map @db "hashtags" {:text (:text %)})))
                              (into #{})
                              vec)
                         nil)]
              (store-publication uid _id nil :reply hids created_at)))]
    (let [retweets (mc/find-maps @db "tweets" {:created_at {$gt (t/date-time 2014 8 1)
                                                            $lt (t/date-time 2014 9 1)}
                                               :in_reply_to_status_id_str {$ne nil}
                                               :user.screen_name {$nin news-accounts}})]
      (time (doall (pmap transact-publications retweets)))))



  ;; export shares
  (letfn [(transact-publications [{:keys [user _id created_at entities id_str in_reply_to_status_id_str] :as tweet}]
            (let [uid (:_id (mc/find-one-as-map @db "users" {:id (:id user)}))
                  hids (if-not (empty? (:hashtags entities))
                         (->> (:hashtags entities)
                              (pmap #(:_id (mc/find-one-as-map @db "hashtags" {:text (:text %)})))
                              (into #{})
                              vec)
                         nil)]
              (store-publication uid _id nil :share hids created_at)))]
    (time
     (doall
      (->> (mc/find-maps @db "origins" {:source nil
                                        :article {$ne nil}
                                        :ts {$gt (t/date-time 2014 8 1)
                                             $lt (t/date-time 2014 9 1)}})
           (pmap (fn [{:keys [tweet]}] (mc/find-map-by-id @db "tweets" tweet)))
           (pmap (fn [{:keys [in_reply_to_status_id retweeted_status] :as tweet}]
                   (if (or in_reply_to_status_id retweeted_status)
                     :no-share
                     tweet)))
           (remove #{:no-share})
           (pmap transact-publications)))))



  ;; retweet reactions
  (time
   (do
     (say/say "Computation started")
     (doall
      (pmap (fn [{:keys [tweet _id]}]
             (let [{{:keys [id_str]} :retweeted_status} (mc/find-map-by-id @db "tweets" tweet)
                   {stid :_id} (mc/find-one-as-map @db "tweets" {:id_str id_str})
                   {spid :_id} (if stid (mc/find-one-as-map @db "publications" {:tweet stid})
                                   nil)]
               (store-reaction _id spid)))
           (mc/find-maps @db "publications" {:type :retweet})))
     (say/say "Computation completed!")))


  ;; reply reactions
  (time
   (do
     (say/say "Computation started")
     (doall
      (pmap (fn [{:keys [tweet _id]}]
             (let [{:keys [in_reply_to_status_id_str]} (mc/find-map-by-id @db "tweets" tweet)
                   {stid :_id} (mc/find-one-as-map @db "tweets" {:id_str in_reply_to_status_id_str})
                   {spid :_id} (if stid (mc/find-one-as-map @db "publications" {:tweet stid})
                                   nil)]
               (store-reaction _id spid)))
           (mc/find-maps @db "publications" {:type :reply})))
     (say/say "Computation completed")))


  ;; share reactions
  (time
   (do
     (say/say "Computation started")
     (doall
      (pmap
       (fn [{:keys [tweet _id]}]
         (let [{aid :article} (mc/find-one-as-map @db "origins" {:tweet tweet})
               {source-tweet :tweet} (mc/find-one-as-map @db "origins" {:article aid :source {$ne nil}})]
           (if source-tweet
             (store-reaction _id source-tweet)
             nil)))
       (mc/find-maps @db "publications" {:type :share})))
     (say/say "Computation completed")))


  ;; store mentions
  (time
   (do
     (say/say "Computation started")
     (doall
      (pmap
       (fn [{:keys [_id tweet]}]
         (let [{{:keys [user_mentions]} :entities} (mc/find-map-by-id @db "tweets" tweet)]
           (pmap
            (fn [{:keys [id]}]
              (let [uid (:_id (mc/find-one-as-map @db "users" {:id id}))]
                (if uid
                  (store-mention uid _id)
                  nil)))
            user_mentions)))
       (mc/find-maps @db "publications")))
     (say/say "Computation completed")))

)
