(ns ceres.exporter
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
