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
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clj-time.core :as t]
            [clojure.pprint :refer [pprint]])
 (:import org.bson.types.ObjectId))


(timbre/refer-timbre)

(def stopwords
  (into #{}
        (clojure.string/split
             "aber,als,am,an,auch,auf,aus,bei,bin,bis,ist,da,dadurch,daher,darum,das,daß,dass,dein,deine,dem,den,der,des,dessen,deshalb,die,dies,dieser,dieses,doch,dort,du,durch,ein,eine,einem,einen,einer,eines,er,es,euer,eure,für,habe,hast,hat,haben,habt,hatte,hatten,hattest,hattet,hier,hinter,ich,ihr,ihre,im,in,ist,ja,jede,jedem,jeden,jeder,jedes,jener,jenes,jetzt,kann,kannst,können,könnt,machen,mein,meine,mit,muß,mußt,musst,müssen,müßt,nach,nachdem,nein,ncht,nun,oder,seid,sein,seine,sich,sie,sind,soll,sollen,sollst,sollt,sonst,soweit,sowie,und,unser,unsere,unter,vom,von,vor,wann,warum,was,weiter,weitere,wenn,wer,werde,werden,werdet,weshalb,wie,wieder,wieso,wir,wird,wirst,wo,woher,wohin,zu,zum,zur,über,rt"
             #",")))

(def mongo-state
  (atom
   {:db (let [^MongoOptions opts (mg/mongo-options :threads-allowed-to-block-for-connection-multiplier 300)
              ^ServerAddress sa  (mg/server-address (or (System/getenv "DB_PORT_27017_TCP_ADDR") "127.0.0.1") 27017)]
          (mg/get-db (mg/connect sa opts) "athena"))
    :custom-formatter (f/formatter "E MMM dd HH:mm:ss Z YYYY")
    :news-accounts #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ" "BILD" "DerWesten" "ntvde" "tazgezwitscher" "welt" "ZDFheute" "N24_de" "sternde" "focusonline"}}))


(defn init-mongo []
  (do
    (mc/ensure-index (:db @mongo-state) "articles" (array-map :ts 1))
    (mc/ensure-index (:db @mongo-state) "origins" (array-map :ts 1))
    (mc/ensure-index (:db @mongo-state) "origins" (array-map :source 1))
    (mc/ensure-index (:db @mongo-state) "tweets" (array-map :user.screen_name 1))
    (mc/ensure-index (:db @mongo-state) "tweets" (array-map :id_str 1))
    (mc/ensure-index (:db @mongo-state) "tweets" (array-map :retweeted_status.id_str 1))
    (mc/ensure-index (:db @mongo-state) "tweets" (array-map :in_reply_to_status_id_str 1))
    (mc/ensure-index (:db @mongo-state) "tweets" (array-map :created_at 1))
    (mc/ensure-index (:db @mongo-state) "tweets" (array-map :entities.user_mentions.screen_name 1 :retweeted_status.user.screen_name 1 :in_reply_to_screen_name 1))))

(defrecord Article [source tweet reactions])
(defrecord Reaction [tweet reactions])


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


(defn trace-parent
  "Compute ancestor trace of given tweet"
  [tweet origins]
  (let [replied-id (:in_reply_to_status_id_str tweet)
        retweeted-status-id (-> tweet :retweeted_status :id_str)
        parent (if replied-id
                 (mc/find-one-as-map (:db @mongo-state) "tweets" {:id_str replied-id})
                 (if retweeted-status-id
                   (mc/find-one-as-map (:db @mongo-state) "tweets" {:id_str retweeted-status-id})
                   nil))]
    (if parent
      (trace-parent parent (into origins [(:_id parent)]))
      origins)))


(defn store-url [{:keys [article record ts source]}]
  (mc/insert-and-return
   (:db @mongo-state)
   "urls"
   {:tweet record
    :article article
    :source source
    :ts ts}))


(defn store-origin [{:keys [article record ts source ancestors root]}]
  (mc/insert-and-return
   (:db @mongo-state)
   "origins"
   {:tweet record
    :article article
    :source source
    :ancestors ancestors
    :root root
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
                   (map #(assoc % :article (-> (mc/find-one-as-map (:db @mongo-state) "articles" {:url (:url %)}) :_id)) expanded-urls))
        ancestors (trace-parent record [])]
    (if (nil? articles)
      (do
        (store-origin {:record oid :ts ts :source source :ancestors ancestors :article nil :root (last ancestors)})
        nil)
      (doall
       (map
        #(if (:article %)
           (do (store-origin (assoc % :record oid :ts ts :source source :ancestors ancestors :root (last ancestors)))
               nil)
           (let [article (store-article (assoc % :ts ts))
                 origin (store-origin (assoc % :article (:_id article) :record oid :ts ts :source source :ancestors ancestors :root (last ancestors)))]
             {:article (update-in article [:ts] (fn [x] (f/unparse (:custom-formatter @mongo-state) x))) :origin (str (:_id origin))}))
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
  (vec (pmap #(vec [% (mc/count (:db @mongo-state) "tweets" {:user.screen_name %
                                                             :created_at {$gt (t/date-time 2014 7 1)}})]) (:news-accounts @mongo-state))))


(defn get-tweet-count []
  (mc/count (:db @mongo-state) "tweets" {:created_at {$gt (t/date-time 2014 7 1)}}))


(defn get-tweets-from-date [month day]
  (mc/find-maps (:db @mongo-state) "tweets"
                {:created_at {$gt (t/date-time 2014 month day 0 0 0 0)
                              $lte (t/date-time 2014 month day 23 59 59 999)}}))


(defn get-articles-from-date [month day]
  (mc/find-maps (:db @mongo-state) "articles"
                {:ts {$gt (t/date-time 2014 month day 0 0 0 0)
                      $lte (t/date-time 2014 month day 23 59 59 999)}}))


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


(defn compute-tweet-diffusion [tweet-id parents]
  (let [tweet (mc/find-one-as-map (:db @mongo-state) "tweets" {:id_str tweet-id})
        neighbor-tweets (->> (mc/find-maps
                              (:db @mongo-state)
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]}
                              [:text :user.screen_name :id_str :in_reply_to_status_id_str :retweeted_status.id_str])
                             (pmap #(assoc % :parents (into parents [(:_id tweet)]))))
        neightbar-ids (pmap :id_str neighbor-tweets)]
    (merge neighbor-tweets (pmap #(compute-tweet-diffusion % (into parents [(:_id tweet)])) neightbar-ids))))


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
       (pmap #(update-in % [:_id] str))
       (pmap #(update-in % [:created_at] (fn [x] (f/unparse (:custom-formatter @mongo-state) x))))
       (pmap str)
       (clojure.string/join "\n")))


(defn export-articles
  [m d]
  (->> (get-articles-from-date m d)
       (pmap #(update-in % [:_id] str))
       (pmap #(update-in % [:ts] (fn [x] (f/unparse (:custom-formatter @mongo-state) x))))
       (pmap str)
       (clojure.string/join "\n")))


(defn get-recent-articles []
  (->> (mc/find-maps (:db @mongo-state) "articles" {:ts {$gt (t/date-time 2014 7 20)}})
       (pmap #(dissoc % :html :_id))
       vec))


(defn get-articles-count []
  (mc/count (:db @mongo-state) "articles"))


(defn find-source
  "Find the source of a given article"
  [id]
  (mc/find-maps (:db @mongo-state) "urls" {$and [{:article id} {:source {$ne nil}}]} [:source :tweet :ts]))


(defn spread
  "Find all tweets that are related to a given tweet by a retweet or reply"
  [tweet]
  (let [tweet-id (:id_str tweet)
        neighbor-tweets (->> (mc/find-maps
                              (:db @mongo-state)
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]}))]
    (Reaction. tweet (vec (doall (pmap spread neighbor-tweets))))))


(defn compute-impact-graph
  "Create the impact graph using clojure zippers"
  [origin]
  (let [article (if (:article origin)
                      (mc/find-map-by-id (:db @mongo-state) "articles" (:article origin))
                      nil)
        tweet (mc/find-map-by-id (:db @mongo-state) "tweets" (:tweet origin))]
    (let [related-origins (if article
                             (->> (mc/find-maps (:db @mongo-state) "origins" {:article  (:_id article)} [:tweet])
                                  (remove #(= (:_id %) (:_id origin)))
                                  (pmap :tweet)
                                  (pmap #(mc/find-map-by-id (:db @mongo-state) "tweets" %))
                                  (filter #(or (nil? (-> % :retweeted_status :id_str)) (= (-> % :retweeted_status :id_str) (:id_str tweet)))))
                             nil)
          related-tweets (->> (mc/find-maps
                                (:db @mongo-state)
                                "tweets"
                                {$and [{$or [{"retweeted_status.id_str" (:id_str tweet)}
                                             {"in_reply_to_status_id_str" (:id_str tweet)}]}
                                       {:created_at {$gt (t/date-time 2014 7 1)}}]}))
          merged-articles (into related-origins related-tweets)]
      (zip/zipper
       (fn [node] true)
       (fn [node] (:reactions node))
       (fn [node new-children] (assoc-in node [:reactions] new-children))
       (Article. origin tweet (vec (pmap spread (into #{} merged-articles))))))))


(defn compute-dfs
  "Compute the amount of elements in the impact graph"
  [graph]
  (loop [counter 0
         loc graph]
    (if (zip/end? loc)
      counter
      (recur
       (if (nil? (zip/node loc))
         counter
         (inc counter))
       (zip/next loc)))))


(defn simplify-graph
  "Shows only specific elements in the nodes of an impact graph"
  [graph]
  (loop [pub-time (-> (zip/root graph) :source :ts)
         hashtags []
         tokens []
         loc graph]
    (if (zip/end? loc)
      {:nodes(zip/root loc)
       :tokens (frequencies (remove #(= % "") tokens))
       :hashtags (frequencies (map :text hashtags))}
      (recur
       pub-time
       (if-let [node (zip/node loc)]
         (->> node :tweet :entities :hashtags (into hashtags))
         hashtags)
       (if (zip/node loc)
         (let [text (clojure.string/lower-case (clojure.string/replace (-> loc zip/node :tweet :text) #"(\n|\d|\t|\,|\.)" " "))]
           (->> (clojure.string/split text  #" ")
                (remove #(contains? stopwords %))
                (into tokens)))
         tokens)
       (if (nil? (zip/node loc))
         (zip/next loc)
         (zip/next
          (zip/edit
           loc
           (fn [x]
             (update-in
              x
              [:tweet]
              #(-> {:text (-> % :text)
                    :id (-> % :id_str)
                    :user (-> % :user :screen_name)
                    :reply (-> % :in_reply_to_status_id_str)
                    :delay (let [post-delay (if (t/after? (-> % :created_at) pub-time)
                                              (t/interval pub-time (-> % :created_at))
                                              (t/interval (-> % :created_at) pub-time))]
                             [(t/in-days post-delay)
                              (t/in-hours post-delay)
                              (t/in-minutes post-delay)
                              (t/in-seconds post-delay)])
                    :rt (-> % :retweeted_status :id_str)}))))))))))


(defn tree-height
  "Computes the height of a given impact tree, finding the longest path from root to a leaf node"
  [tree]
  (loop [max-path 0
         loc tree]
    (if (zip/end? loc)
      max-path
      (recur
       (if (zip/node loc)
         (-> loc
             zip/path
             count
             (max max-path))
         max-path)
       (zip/next loc)))))


(defn example-graph []
  (let [tree (compute-impact-graph (mc/find-map-by-id (:db @mongo-state) "origins" (ObjectId. "53de1541657a439ad20d6859")))] ; alternativ "53de3d68657a74caed255892" "53d7a7ad657ad4126658d0ba" "53de089f657a439ad20d6133"
    {:graph (simplify-graph tree)
     :height (tree-height tree)}))


(defn random-graph []
  (let [tree (-> (mc/find-maps (:db @mongo-state) "origins" {:source {$in (:news-accounts @mongo-state)}}) rand-nth compute-impact-graph)]
    {:graph (simplify-graph tree)
     :height (tree-height tree)
     :size (compute-dfs tree)}))


(comment

  ;; TODO update on server
  (time
   (doseq [x (monger.collection/find-maps (:db @mongo-state) "tweets")]
     (mc/update-by-id
      (:db @mongo-state)
      "tweets"
      (:_id x)
      (update-in x [:created_at] #(f/parse (:custom-formatter @mongo-state) (:created_at %))))))

  (def articles (mc/find-maps (:db @mongo-state) "origins" {:source {$in (:news-accounts @mongo-state)}}))

  (def example-graph (compute-impact-graph (mc/find-map-by-id (:db @mongo-state) "origins" (ObjectId. "53da170d657a10b9f098be86"))))

  (-> (let [tree (-> articles rand-nth compute-impact-graph)]
        [(simplify-graph tree)
         (tree-height tree)])
       clojure.pprint/pprint)

  (count articles)

  (-> (mc/find-map-by-id (:db @mongo-state) "origins" (ObjectId. "53de1541657a439ad20d6859"))
      compute-impact-graph
      simplify-graph
      clojure.pprint/pprint)

  (let [trees (pmap compute-impact-graph articles)]
    (->> (pmap tree-height trees)
         frequencies
         clojure.pprint/pprint))

  (pprint (random-graph))

)
