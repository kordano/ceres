(ns ceres.curator
  (:refer-clojure :exclude [sort find])
  (:require [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.query :refer :all]
            [clojure.data.json :as json]
            [monger.joda-time]
            [clojure.string :refer [split join]]
            [taoensso.timbre :as timbre]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [ceres.collector :refer [db custom-formatter news-accounts store]])
 (:import org.bson.types.ObjectId))


(timbre/refer-timbre)


(defrecord Article [source tweet reactions])
(defrecord Reaction [tweet reactions])


(def stopwords
  (into #{}
        (clojure.string/split
             "aber,als,am,an,auch,auf,aus,bei,bin,bis,ist,da,dadurch,daher,darum,das,daß,dass,dein,deine,dem,den,der,des,dessen,deshalb,die,dies,dieser,dieses,doch,dort,du,durch,ein,eine,einem,einen,einer,eines,er,es,euer,eure,für,habe,hast,hat,haben,habt,hatte,hatten,hattest,hattet,hier,hinter,ich,ihr,ihre,im,in,ist,ja,jede,jedem,jeden,jeder,jedes,jener,jenes,jetzt,kann,kannst,können,könnt,machen,mein,meine,mit,muß,mußt,musst,müssen,müßt,nach,nachdem,nein,ncht,nun,oder,seid,sein,seine,sich,sie,sind,soll,sollen,sollst,sollt,sonst,soweit,sowie,und,unser,unsere,unter,vom,von,vor,wann,warum,was,weiter,weitere,wenn,wer,werde,werden,werdet,weshalb,wie,wieder,wieso,wir,wird,wirst,wo,woher,wohin,zu,zum,zur,über,rt"
             #",")))


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


;; --- DATA MINING ---

(defn get-news-frequencies []
  (vec
   (pmap
    #(-> [%
          (mc/count
           db "tweets"
           {:user.screen_name %
            :created_at {$gt (t/date-time 2014 7 1)}})])
    news-accounts)))


(defn get-tweet-count []
  (mc/count db "tweets" {:created_at {$gt (t/date-time 2014 7 1)}}))


(defn get-tweets-from-date [year month day]
  (mc/find-maps db "tweets"
                {:created_at {$gt (t/date-time year month day 0 0 0 0)
                              $lte (t/date-time year month day 23 59 59 999)}}))


(defn get-articles-from-date [year month day]
  (mc/find-maps db "articles"
                {:ts {$gt (t/date-time year month day 0 0 0 0)
                      $lte (t/date-time year month day 23 59 59 999)}}))


(defn compute-diffusion [user]
  (->> (mc/count db "tweets" {$and [{$or [{"entities.user_mentions.screen_name" user}
                                                           {"retweeted_status.user.screen_name" user}
                                                           {"in_reply_to_screen_name" user}]}
                                                    {:created_at {$gt (t/date-time 2014 7 1)}}]})))


(defn compute-tweet-diffusion [tweet-id parents]
  (let [tweet (mc/find-one-as-map db "tweets" {:id_str tweet-id})
        neighbor-tweets (->> (mc/find-maps
                              db
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]}
                              [:text :user.screen_name :id_str :in_reply_to_status_id_str :retweeted_status.id_str])
                             (pmap #(assoc % :parents (into parents [(:_id tweet)]))))
        neightbar-ids (pmap :id_str neighbor-tweets)]
    (merge neighbor-tweets (pmap #(compute-tweet-diffusion % (into parents [(:_id tweet)])) neightbar-ids))))


(defn get-news-diffusion []
  (mapv #(vec [% (compute-diffusion %)]) news-accounts))


(defn get-month-distribution [month]
  (let [day-range (months (dec month))]
    (vec
     (pmap
      (fn [day]
        (into {} [[:date (str (t/date-time 2014 month day))]
                  [:count
                   (mc/count
                    db
                    "tweets"
                    {:created_at
                     {$gt (t/date-time 2014 month day 0 0 0 0)
                      $lte (t/date-time 2014 month day 23 59 59 999)}})]]))
      day-range))))

(defn get-recent-articles []
  (->> (mc/find-maps db "articles" {:ts {$gt (t/date-time 2014 7 20)}})
       (pmap #(dissoc % :html :_id))
       vec))


(defn get-articles-count []
  (mc/count db "articles"))


(defn find-source
  "Find the source of a given article"
  [id]
  (mc/find-maps db "urls" {$and [{:article id} {:source {$ne nil}}]} [:source :tweet :ts]))


(defn spread
  "Find all tweets that are related to a given tweet by a retweet or reply"
  [tweet]
  (let [tweet-id (:id_str tweet)
        neighbor-tweets (->> (mc/find-maps
                              db
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]}))]
    (Reaction. tweet (vec (doall (pmap spread neighbor-tweets))))))


(defn compute-impact-graph
  "Create the impact graph using clojure zippers"
  [origin]
  (let [article (if (:article origin)
                      (mc/find-map-by-id db "articles" (:article origin))
                      nil)
        tweet (mc/find-map-by-id db "tweets" (:tweet origin))]
    (let [related-origins (if article
                             (->> (mc/find-maps db "origins" {:article  (:_id article)} [:tweet])
                                  (remove #(= (:_id %) (:_id origin)))
                                  (pmap :tweet)
                                  (pmap #(mc/find-map-by-id db "tweets" %))
                                  (filter #(or (nil? (-> % :retweeted_status :id_str)) (= (-> % :retweeted_status :id_str) (:id_str tweet)))))
                             nil)
          related-tweets (->> (mc/find-maps
                                db
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
  (let [tree (compute-impact-graph (mc/find-map-by-id db "origins" (ObjectId. "53de1541657a439ad20d6859")))] ; alternativ "53de3d68657a74caed255892" "53d7a7ad657ad4126658d0ba" "53de089f657a439ad20d6133"
    {:graph (simplify-graph tree)
     :height (tree-height tree)
     :size (compute-dfs tree)}))


(defn random-graph []
  (let [tree (-> (mc/find-maps db "origins" {:source {$in news-accounts}}) rand-nth compute-impact-graph)]
    {:graph (simplify-graph tree)
     :height (tree-height tree)
     :size (compute-dfs tree)}))


;; --- DATA EXPORT/IMPORT ---

;;todo check if id exists in database
(defn read-data
  "Reads in json data from given path and stores it"
  [path]
  (doall (map #(let [data (json/read-str % :key-fn keyword)]
                 (println "Importing " (:id data))
                 (store data)) (split (slurp path) #"\n"))))


(defn export-tweets
  "Export all collected tweets from a specific date as edn file. Read https://github.com/edn-format/edn for edn format details."
  [y m d]
  (->> (get-tweets-from-date y m d)
       (pmap #(update-in % [:_id] str))
       (pmap #(update-in % [:created_at] (fn [x] (f/unparse custom-formatter x))))
       (pmap str)
       (clojure.string/join "\n")))


(defn backup-tweets [folder-path]
  (let [date (t/minus (t/today) (t/days 1))
        y (t/year date)
        m (t/month date)
        d (t/day date)
        data (export-tweets y m d)
        file-path (str folder-path "/tweets-" y "-" m "-" d ".edn")]
    (spit file-path data)))


(defn export-articles
  [y m d]
  (->> (get-articles-from-date y m d)
       (pmap #(update-in % [:_id] str))
       (pmap #(update-in % [:ts] (fn [x] (f/unparse custom-formatter x))))
       (pmap str)
       (clojure.string/join "\n")))


(comment

  ;; TODO update on server
  (time
   (doseq [x (monger.collection/find-maps db "tweets")]
     (mc/update-by-id
      db
      "tweets"
      (:_id x)
      (update-in x [:created_at] #(f/parse custom-formatter (:created_at %))))))

  (def articles (mc/find-maps db "origins" {:source {$in news-accounts}}))

  (def example-graph (compute-impact-graph (mc/find-map-by-id db "origins" (ObjectId. "53da170d657a10b9f098be86"))))

  (-> (let [tree (-> articles rand-nth compute-impact-graph)]
        [(simplify-graph tree)
         (tree-height tree)])
       clojure.pprint/pprint)

  (count articles)

  (-> (mc/find-map-by-id db "origins" (ObjectId. "53de1541657a439ad20d6859"))
      compute-impact-graph
      simplify-graph
      clojure.pprint/pprint)

  (let [trees (pmap compute-impact-graph articles)]
    (->> (pmap tree-height trees)
         frequencies
         clojure.pprint/pprint))

)
