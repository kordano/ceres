(ns ceres.collector
 (:import [twitter4j
              StatusListener TwitterStream TwitterStreamFactory
              FilterQuery]
           [twitter4j.conf ConfigurationBuilder Configuration]
           [twitter4j.json DataObjectFactory]))

(set! *warn-on-reflection* true)


(defn config-with-password
  "Twitter stream config"
  ^Configuration []
  (let [cb (ConfigurationBuilder.)]
    (.setDebugEnabled cb true)
    (.setOAuthConsumerKey cb "RfwfMlqMXqWnIofQ8QjU5TpSX")
    (.setOAuthConsumerSecret cb "tXF0cJM0ltTyMw1363cNWAkflbgzg0LBeFrutFer7E9ksSZaJz")
    (.setOAuthAccessToken cb "108654757-1jR2QjJj3gZINhT7aTdQGKX0pKf3yIKyQGSu322w")
    (.setOAuthAccessTokenSecret cb "nbEkOMtOM6Ped8xozXHo6j2sI82k1uH1yOyZPMzuoOcng")
    (.setJSONStoreEnabled cb true)
    (.build cb)))



(defn status-listener
  "Stream handler, currently prints status of a new tweet"
  []
  (proxy [StatusListener] []

    (onStatus [^twitter4j.Status status]
      (println (.getText status))
      ;(println (DataObjectFactory/getRawJSON status)
    )

    (onException [^java.lang.Exception e] (.printStackTrace e))
    (onDeletionNotice [^twitter4j.StatusDeletionNotice statusDeletionNotice] ())
    (onScrubGeo [userId upToStatusId] ())
    (onTrackLimitationNotice [numberOfLimitedStatuses] ())))


(defn get-twitter-stream-factory
  "Creates the twitter factory for the stream object"
  []
  (let [factory (TwitterStreamFactory. (config-with-password))]
    (.getInstance factory)))


(defn do-filter-stream
  "Starts streaming, following given ids and tracking given keywords"
  [ids keywords]
  (let [filter-query (FilterQuery. 0 (long-array ids) (into-array String keywords))
        stream (get-twitter-stream-factory)]
    (.addListener stream (status-listener))
    (.filter stream filter-query)))

(comment

  (do-filter-stream [114508061 18016521 5734902 40227292 2834511] ["@FAZ_NET" "@tagesschau" "@dpa" "@SZ" "@SPIEGELONLINE"])

  )
