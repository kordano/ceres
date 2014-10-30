(ns ceres.db
  (:require [datomic.api :as d]
            [clojure.java.io :as io]
            [net.cgrand.enlive-html :as enlive]
            [ceres.collector :refer [db expand-url store-article store-origin custom-formatter news-accounts]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.conversion :refer [from-db-object]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [aprint.core :refer [aprint]])
  (:import datomic.Util))

(def db-uri-base "datomic:free://0.0.0.0:4334")

(defn- scratch-conn
  "Create a connection to an anonymous, in-memory database."
  []
  (let [uri (str "datomic:mem://" (d/squuid))]
    (d/delete-database uri)
    (d/create-database uri)
    (d/connect uri)))


(defn db-conn []
  (let [uri (str db-uri-base "/ceres")]
    (d/create-database uri)
    (d/connect uri)))


(defn read-all
  "Read all forms in f, where f is any resource that can
   be opened by io/reader"
  [f]
  (Util/readAll (io/reader f)))

(defn transact-all
  "Load and run all transactions from f, where f is any
   resource that can be opened by io/reader."
  [conn f]
  (doseq [txd (read-all f)]
    (d/transact conn txd))
  :done)


(defn init-schema [conn path]
  (transact-all conn (io/resource path)))


(defn transact-hashtag [conn hashtag]
  (let [tid (d/tempid :db.part/user)]
      (d/transact
       conn
       [{:db/id tid
         :hashtag/name hashtag}])))

(defn transact-user [conn {:keys [screen_name created_at id followers_count] :as user}]
  (aprint screen_name)
  (let [tid (d/tempid :db.part/user)]
      (d/transact
       conn
       [{:db/id tid
         :user/screen-name screen_name
         :user/id id
         :user/created (c/to-date (f/parse custom-formatter created_at))
         :user/followers followers_count}])))

(defn get-user-id [conn id]
  (let [query '[:find ?e
               :in $ ?id
               :where
                [?e :user/id ?id]]
        db (d/db conn)]
    (ffirst (d/q query db id))))

(defn add-user [conn user]
  (aprint [:add-user (:id user)])
  (transact-user conn user)
  (get-user-id conn (:id user)))

(defn add-hashtag [conn hashtag]
  (-> (transact-hashtag conn hashtag)
      deref
      :tempids
      vals
      first))

(defn get-hashtag-id [conn text]
  (let [query '[:find ?e
               :in $ ?text
               :where
                [?e :hashtag/name ?text]]
        db (d/db conn)]
    (ffirst (d/q query db text))))

(defn transact-tweet [conn tweet url type uid hids mids rids]
  (let [tid (d/tempid :db.part/user)]
      (d/transact
       conn
       [{:db/id tid
         :tweet/user uid
         :tweet/text (:text tweet)
         :tweet/type type
         :tweet/id (:id tweet)
         :tweet/url url
         :tweet/hashtags hids
         :tweet/mentions mids
         :tweet/reactions rids}])
      tid))

(defn get-tweet-id [conn id]
  (let [query '[:find ?e
               :in $ ?id
               :where
                [?e :tweet/id ?id]]
        db (d/db conn)]
    (ffirst (d/q query db id))))

(defn add-tweet [conn tweet url type uid hids mids rids]
  (aprint [:add-tweet (:id tweet)])
  (transact-tweet conn tweet url type uid hids mids rids)
  (get-tweet-id conn (:id tweet)))


(comment

  (def conn (scratch-conn))

  (init-schema conn "schema.edn")

  (transact-hashtag conn "haskell")

  (d/q '[:find ?email
          :where
          [?e :tweet/text ?email]]
        (d/db conn))



  (letfn [(dispatch-this [{:keys [id_str retweeted_status in_reply_to_status_id_str] :as tweet}]
               (if in_reply_to_status_id_str
                 :tweet.type/reply
                 (if (:id_str retweeted_status)
                   :tweet.type/retweet
                   :tweet.type/share)))
          (find-related-tweets [{:keys [id id_str user entities] :as tweet}]
            (let [tweets (mc/find-maps @db "tweets"
                                 {$or [{"retweeted_status.id_str" (:id_str tweet)}
                                       {"in_reply_to_status_id_str" (:id_str tweet)}]})
                  uid (if-let [db-uid (get-user-id conn (:id user))]
                        db-uid
                        (add-user conn user))
                  hids (if (empty? (:hashtags entities))
                         []
                         (pmap
                          #(if-let [hid (-> % :text get-hashtag-id)]
                             hid
                             (add-hashtag conn %))
                          (:hashtags entities)))
                  mids (if (empty? (:user_mentions entities))
                         []
                         (pmap #(-> % :id get-user-id) (:user_mentions entities)))
                  rids (if (empty? (empty? tweets))
                         []
                         (pmap find-related-tweets tweets))]
              (add-tweet conn tweet "" (dispatch-this tweet) uid hids mids rids)))
          (direct-reactions [origin]
            (let [article (if (:article origin)
                            (mc/find-map-by-id @db "articles" (:article origin))
                            nil)
                  {:keys [id id_str user entities] :as tweet} (mc/find-map-by-id @db "tweets" (:tweet origin))]
              (let [related-origins (if article
                                      (->> (mc/find-maps @db "origins" {:article  (:_id article)} [:tweet])
                                           (remove #(= (:_id %) (:_id origin)))
                                           (pmap :tweet)
                                           (pmap #(mc/find-map-by-id @db "tweets" %))
                                           (filter #(or
                                                     (nil? (-> % :retweeted_status :id_str))
                                                     (= (-> % :retweeted_status :id_str)
                                                        (:id_str tweet)))))
                                      nil)
                    related-tweets (mc/find-maps @db "tweets"
                                                 {$or [{"retweeted_status.id_str" (:id_str tweet)}
                                                       {"in_reply_to_status_id_str" (:id_str tweet)}]})
                    merged-tweets (into related-origins related-tweets)
                    uid (if-let [db-uid (get-user-id conn (:id user))]
                          db-uid
                          (add-user conn user))
                    hids (if (empty? (:hashtags entities))
                           []
                           (map
                            #(if-let [hid (-> % :text get-hashtag-id)]
                               hid
                               (add-hashtag conn %))
                            (:hashtags entities)))
                    mids (if (empty? (:user_mentions entities))
                           []
                           (map #(-> % :id get-user-id) (:user_mentions entities)))
                    rids (if (empty? merged-tweets)
                           []
                           (map find-related-tweets merged-tweets))]
                (add-tweet conn tweet (:url article) :tweet.type/source uid hids mids rids))))]
    (->> (mc/find-maps @db "origins" {:ts {$gt (t/date-time 2014 8 1)
                                           $lt (t/date-time 2014 8 2)}
                                      :source {$ne nil}})
         first
         direct-reactions
         aprint))


  (println "\n")





)
