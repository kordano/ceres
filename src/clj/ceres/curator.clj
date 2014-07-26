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


(defn fetch-url [url]
  (enlive/html-resource (java.net.URL. url)))


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
   (:db @mongo-state)
   "urls"
   {:tweet record
    :article article
    :source source
    :ts ts}))


(defn store-article [{:keys [url content-type ts] :as expanded-url}]
  (let [raw-html (slurp url)
        html-title (-> (java.io.StringReader. raw-html) enlive/html-resource (enlive/select [:head :title]) first :content first)]
    (mc/insert-and-return
     (:db @mongo-state)
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
        doc (update-in tweet [:created_at] (fn [x] (f/parse (:custom-formatter @mongo-state) x)))
        record (from-db-object (mc/insert-and-return (:db @mongo-state) "tweets" (merge doc {:_id oid})) true)
        ts (:created_at record)
        source ((:news-accounts @mongo-state) (-> record :user :screen_name))
        record-urls (-> record :entities :urls)
        expanded-urls (if (empty? record-urls)
                        nil
                        (map #(let [expanded-url (expand-url (:expanded_url %))]
                                (if (:url expand-url)
                                  expanded-url
                                  (assoc expanded-url :url (:expanded_url %)))) record-urls))
        articles (if (nil? expanded-urls)
                   nil
                   (map #(assoc % :article (-> (mc/find-one-as-map (:db @mongo-state) "articles" {:url (:url %)}) :_id)) expanded-urls))]
    (if (nil? articles)
      nil
      (doall
       (map
        #(if (:article %)
           (do (store-url (assoc % :record oid :ts ts :source source))
               (update-in (mc/find-map-by-id (:db @mongo-state) "articles" (:article %)) [:ts] (fn [x] (f/unparse (:custom-formatter @mongo-state) x))))
           (let [article (store-article (assoc % :ts ts))]
             (do (store-url (assoc % :article (:_id article) :record oid :ts ts :source source))
                 (do (store-url (assoc % :record oid :ts ts :source source))
                     (update-in article [:ts] (fn [x] (f/unparse (:custom-formatter @mongo-state) x)))))))
        articles)))))


;;todo check if id exists in database
(defn read-data
  "Reads in json data from given path and stores it"
  [path]
  (doall (map #(let [data (json/read-str % :key-fn keyword)]
                 (println "Importing " (:id data))
                 (store data)) (split (slurp path) #"\n"))))


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


(defn export-edn
  "Export all collected tweets from a specific date as edn file. Read https://github.com/edn-format/edn for edn format details."
  [m d]
  (->> (get-tweets-from-date m d)
       (map #(dissoc % :_id))
       (map #(update-in % [:created_at] (fn [x] (f/unparse (:custom-formatter @mongo-state) x))))
       (map str)
       (clojure.string/join "\n")))

(defn get-recent-articles []
  (->> (mc/find-maps (:db @mongo-state) "articles" {:ts {$gt (t/date-time 2014 7 20)}})
       (pmap #(dissoc % :html :_id))
       vec))

(defn get-articles-count []
  (mc/count (:db @mongo-state) "articles"))


(comment

  ;; TODO update on server
  (time
   (doseq [x (monger.collection/find-maps (:db @mongo-state) "tweets")]
     (mc/update-by-id
      (:db @mongo-state)
      "tweets"
      (:_id x)
      (update-in x [:created_at] #(f/parse (:custom-formatter @mongo-state) (:created_at %))))))

  (->> (mc/find (:db @mongo-state) "articles")
       seq
       (map #(from-db-object % true)))

  (let [raw-html (slurp "http://www.tagesschau.de/multimedia/bilder/mh17-bergung-120.html")]
    (type raw-html)
    (-> (enlive/html-resource (java.io.StringReader. raw-html))
        clojure.pprint/pprint))

  (take 10 (sort-by val > (-> (mc/find (:db @mongo-state) "articles")
                              seq
                              last
                              (from-db-object true)
                              :html
                              (clojure.string/split #"\s")
                              (into #{})
                              frequencies)))

  (->> (mc/find-maps (:db @mongo-state) "urls" {:source "SPIEGELONLINE"})
       (map #(mc/find-maps (:db @mongo-state) "urls" {:article (:article %)}))
       flatten
       count)

  (->> (mc/find-maps (:db @mongo-state) "urls")
       (map :article)
       frequencies
       (sort-by val >)
       (take 10)
       (map #(vec [(:title (mc/find-map-by-id (:db @mongo-state) "articles" (key %))) %])))

)
