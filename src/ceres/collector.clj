(ns ceres.collector
  (:require [clojure.data.json :as json]
            [ceres.warehouse :as warehouse]
            [ceres.curator :as curator])
  (:import [twitter4j StatusListener TwitterStream TwitterStreamFactory FilterQuery]
           [twitter4j.conf ConfigurationBuilder Configuration]
           [twitter4j.json DataObjectFactory]))

(set! *warn-on-reflection* true)

(def twitter-config
  {:consumer-key "RfwfMlqMXqWnIofQ8QjU5TpSX"
   :consumer-secret "tXF0cJM0ltTyMw1363cNWAkflbgzg0LBeFrutFer7E9ksSZaJz"
   :access-token "108654757-1jR2QjJj3gZINhT7aTdQGKX0pKf3yIKyQGSu322w"
   :access-token-secret "nbEkOMtOM6Ped8xozXHo6j2sI82k1uH1yOyZPMzuoOcng"})

(defn config-with-password
  "Twitter stream config"
  ^Configuration [{:keys [consumer-key consumer-secret access-token access-token-secret]}]
  (let [cb (ConfigurationBuilder.)]
    (.setDebugEnabled cb true)
    (.setOAuthConsumerKey cb consumer-key)
    (.setOAuthConsumerSecret cb consumer-secret)
    (.setOAuthAccessToken cb access-token)
    (.setOAuthAccessTokenSecret cb access-token-secret)
    (.setJSONStoreEnabled cb true)
    (.build cb)))


(defn status-listener
  "Stream handler, currently prints status of a new tweet"
  []
  (proxy [StatusListener] []
    (onStatus [^twitter4j.Status status]
      (let [tweet (json/read-str (DataObjectFactory/getRawJSON status) :key-fn keyword)]
        (curator/store-tweet tweet)
        (println  (str "[" (:created_at tweet) "] Storing " (:id tweet) " from " (:screen_name (:user tweet))))))
    (onException [^java.lang.Exception e] (.printStackTrace e))
    (onDeletionNotice [^twitter4j.StatusDeletionNotice statusDeletionNotice] ())
    (onScrubGeo [userId upToStatusId] ())
    (onTrackLimitationNotice [numberOfLimitedStatuses] ())))


(defn get-twitter-stream-factory
  "Creates the twitter factory for the stream object"
  []
  (let [factory (TwitterStreamFactory. (config-with-password twitter-config))]
    (.getInstance factory)))


(defn do-filter-stream
  "Starts streaming, following given ids and tracking given keywords"
  [ids keywords]
  (let [filter-query (FilterQuery. 0 (long-array ids) (into-array String keywords))
        stream (get-twitter-stream-factory)]
    (.addListener stream (status-listener))
    (.filter stream filter-query)))

(comment

  (def stream (do-filter-stream [114508061 18016521 5734902 40227292 2834511] ["@FAZ_NET" "@tagesschau" "@dpa" "@SZ" "@SPIEGELONLINE"]))

  )
