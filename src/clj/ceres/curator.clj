(ns ceres.curator
  (:refer-clojure :exclude [sort find])
  (:require [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.query :refer :all]
            [clojure.data.json :as json]
            [monger.joda-time]
            [clojure.string :refer [split join lower-case]]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [aprint.core :refer [aprint]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clj-time.periodic :as p]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :refer [sh]]
            [opennlp.nlp :refer [make-tokenizer make-detokenizer]]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [loom.graph :as lg]
            [loom.io :as lio]
            [ceres.collector :refer [db custom-formatter news-accounts store]])
 (:import org.bson.types.ObjectId))


(defrecord Source [name articles])
(defrecord Article [source tweet reactions])
(defrecord Reaction [tweet reactions])

(def tokenize (make-tokenizer "data/de-token.bin"))
(def stopwords (read-string "data/stopwords.txt"))


;; --- DATA MINING ---

(defn get-news-frequencies
  "Compute distribution of news tweets"
  []
  (vec
   (pmap
    #(-> [%
          (mc/count
           @db "tweets"
           {:user.screen_name %
            :created_at {$gt (t/date-time 2014 7 1)}})])
    news-accounts)))


(defn get-tweet-count
  "Compute overall tweet count"
  []
  (mc/count @db "tweets" {:created_at {$gt (t/date-time 2014 7 1)}}))


(defn get-tweets-from-date
  "Fetch tweets from specific date"
  [year month day]
  (mc/find-maps @db "tweets"
                {:created_at {$gt (t/date-time year month day 0 0 0 0)
                              $lte (t/date-time year month day 23 59 59 999)}}))


(defn get-articles-from-date
  "Fetch articles from specific date"
  [year month day morning?]
  (mc/find-maps @db "articles"
                {:ts {$gt (t/date-time year month day (if morning? 0 12) 0 0 0)
                      $lte (t/date-time year month day (if morning? 11 23) 59 59 999)}}))


(defn compute-tweet-diffusion [tweet-id parents]
  (let [tweet (mc/find-one-as-map @db "tweets" {:id_str tweet-id})
        neighbor-tweets (->> (mc/find-maps
                              @db
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]}
                              [:text :user.screen_name :id_str :in_reply_to_status_id_str :retweeted_status.id_str])
                             (pmap #(assoc % :parents (into parents [(:_id tweet)]))))
        neightbar-ids (pmap :id_str neighbor-tweets)]
    (merge neighbor-tweets (pmap #(compute-tweet-diffusion % (into parents [(:_id tweet)])) neightbar-ids))))


(defn get-articles-count
  "Compute articles count"
  []
  (mc/count @db "articles"))


(defn spread
  "Find all tweets that are related to a given tweet by a retweet or reply"
  [tweet]
  (let [tweet-id (:id_str tweet)
        neighbor-tweets (->> (mc/find-maps
                              @db
                              "tweets"
                              {$and [{$or [{"retweeted_status.id_str" tweet-id}
                                           {"in_reply_to_status_id_str" tweet-id}]}
                                     {:created_at {$gt (t/date-time 2014 7 1)}}]}))]
    (Reaction. tweet (vec (doall (pmap spread neighbor-tweets))))))


(defn compute-impact-tree
  "Create the impact tree using clojure zippers"
  [origin]
  (let [article (if (:article origin)
                      (mc/find-map-by-id @db "articles" (:article origin))
                      nil)
        tweet (mc/find-map-by-id @db "tweets" (:tweet origin))]
    (let [related-origins (if article
                             (->> (mc/find-maps @db "origins" {:article  (:_id article)} [:tweet])
                                  (remove #(= (:_id %) (:_id origin)))
                                  (pmap :tweet)
                                  (pmap #(mc/find-map-by-id @db "tweets" %))
                                  (filter #(or (nil? (-> % :retweeted_status :id_str)) (= (-> % :retweeted_status :id_str) (:id_str tweet)))))
                             nil)
          related-tweets (->> (mc/find-maps
                                @db
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


(defn compute-user-tree [name]
  (let [user-tweets (mc/find-maps @db "tweets" (:user.screen_name name))
        reactions (pmap spread user-tweets)]
    {:user name
     :reactions reactions}))


(defn compute-impact-forest
  "Compute article impact forest of given news source"
  [source]
  (let [source-origins (mc/find-maps @db "origins" {:source source})]
    (vec (pmap compute-impact-tree source-origins))))


(defn analyze-tree [tree]
  (loop [counter 0
         max-path 0
         users []
;;         tokens []
         pub-times []
         hashtags []
         delays []
         loc tree]
    (if (zip/end? loc)
      {:size counter
       :height max-path
       :users users
       ;;:tokens tokens
       :pub-times pub-times
       :hashtags (mapv #(-> % :text clojure.string/lower-case) hashtags)
       :delays delays}
      (recur
       (if (zip/node loc)
         (inc counter)
         counter)
       (if (zip/node loc)
         (-> loc zip/path count (max max-path))
         max-path)
       (if-let [node (zip/node loc)]
         (let [user (-> node :tweet :user)]
             (conj users (:screen_name user)))
         users)
       #_(if (zip/node loc)
         (conj
          tokens
          (clojure.set/difference
           (into #{} (-> loc zip/node :tweet :text tokenize))
           (first tokens)))
         tokens)
       (if-let [node (zip/node loc)]
         (->> node :tweet :created_at (conj pub-times))
         pub-times)
       (if-let [node (zip/node loc)]
         (->> node :tweet :entities :hashtags (into hashtags))
         hashtags)
       (if (zip/node loc)
         (let [pub-time (-> (zip/root tree) :source :ts)
               post-delay (if (t/after? (-> loc zip/node :tweet :created_at) pub-time)
                            (t/interval pub-time (-> loc zip/node :tweet :created_at))
                            (t/interval (-> loc zip/node :tweet :created_at) pub-time))]
           (conj delays (t/in-seconds post-delay)))
         delays)
       (zip/next loc)))))


(defn random-tree []
  (let [origin (rand-nth (mc/find-maps @db "origins" {:source {$in news-accounts}}))
        tree (compute-impact-tree origin)]
    {:title (:title (mc/find-map-by-id @db "articles" (:article origin)))
     :tree tree
     :analysis (analyze-tree tree)}))


(defn compute-summary [source]
  (let [impact-forest (compute-impact-forest source)
        analytics (map analyze-tree impact-forest)
        sizes (map :size analytics)
        pub-times (map :pub-times analytics)
        heights (map :height analytics)
        delays (apply concat (map :delays analytics))
        users (->> analytics (map :users) (apply concat))
        hashtags (->> analytics (map :hashtags) flatten)
        overall-size (reduce + sizes)]
    {:source source
     :article-count (count impact-forest)
     :users users
     :hashtags hashtags
     :pub-times pub-times
     :heights heights
     :sizes sizes
     :delays delays
     :total-impact overall-size
     :avg-impact (float (/ overall-size (count sizes)))
     :no-reactions (float (/ (count (filter #(< % 1) heights)) (count heights)))
     :avg-height (float (/ (reduce + heights) (count heights)))}))


;; --- MONGO DATA EXPORT/IMPORT ---
(defn backup
  "Write backup from given date of a specific collection to a given folder"
  [date database coll folder-path]
  (let [day-after (t/plus date (t/days 1))
        m (str (t/month date))
        d (str (t/day date))
        file-path (str folder-path
                       "/" coll
                       "-" (t/year date)
                       "-" (if (< (count m) 2) (str 0 m) m)
                       "-" (if (< (count d) 2) (str 0 d) d)
                       ".json")]
    (sh "mongoexport"
        "--port" "27017"
        "--host" (or (System/getenv "DB_PORT_27017_TCP_ADDR") "127.0.0.1")
        "--db" database
        "--collection" coll
        "--query" (str "{" (if (= coll "tweets") "created_at" "ts") " : {$gte : new Date(" (c/to-long date) "), $lt : new Date(" (c/to-long day-after) ")}}")
        "--out" file-path)))

(defn backup-yesterday
  "Write last day's collection to specific folder"
  [database coll folder-path]
  (backup (t/minus (t/today) (t/days 1)) database coll folder-path))


(comment

  ;; TODO update on server
  (time
   (doseq [x (mc/find-maps @db "tweets")]
     (mc/update-by-id
      db
      "tweets"
      (:_id x)
      (update-in x [:created_at] #(f/parse custom-formatter (:created_at %))) )))

  (def rand-tree (random-tree))

  (-> rand-tree :analysis :height)

  (def g
    (let [vertices (loop [nodes []
                          loc (:tree rand-tree)]
                     (if (zip/end? loc)
                       nodes
                       (recur
                        (if-let [node (zip/node loc)]
                          (if (= (zip/root loc) node)
                            (conj nodes (-> node :tweet :id_str))
                            (conj nodes [(-> node :tweet :id_str) (-> loc zip/up zip/node :tweet :id_str)]))
                          nodes)
                        (zip/next loc))))]
      (apply graph vertices)))

  (lio/view g)

  (let [user (-> rand-tree second :users last)
        tweets (mc/find-maps @db "tweets" {:user.screen_name user})]
    (->> tweets
         (mapv spread)
         ffirst
         aprint
         time))



  (def sum-tagesschau (compute-summary "tagesschau"))

  (def sum-faz (compute-summary "FAZ_NET"))

  (def sum-sz (compute-summary "SZ"))

  (def sum-spon (compute-summary "SPIEGELONLINE"))

  (def sum-welt (compute-summary "welt"))



  ;; publication time of day
  (let [sz-hours (->> sum-sz
                       :pub-times
                       flatten
                       (map t/hour)
                       frequencies
                       (sort-by key <))]
    (view (line-chart (keys sz-hours) (vals sz-hours))))


  ;; reaction delay
  (let [delays (->>  sum-sz
                     :delays
                     (remove #(= % 0))
                     (remove #(> % 3600))
                     frequencies
                     (sort-by key <))]
    (view (line-chart (keys delays) (vals delays))))


  ;; daily tweet counts
  (let [days (t/in-days (t/interval (t/date-time 2014 8 1) (t/date-time 2014 9 15)))
        dates (take days (p/periodic-seq (t/date-time 2014 8 1) (t/days 1)))
        tweet-counts (map
                      (fn [date]
                        (->> sum-sz
                             :pub-times
                             flatten
                             (filter #(and (t/after? % date) (t/before? % (t/plus date (t/days 1)))))
                             count))
                      dates)]
    (view (line-chart (range 1 (inc days)) tweet-counts)))



  ;; tweet counts
  (let [days-running (t/in-days (t/interval (t/date-time 2014 7 2) (t/date-time 2014 9 15)))
        dates (take days-running (p/periodic-seq (t/date-time 2014 7 2) (t/days 1)))
        sz-tweet-count (map #(mc/count @db "tweets" {:user.screen_name "SZ" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        spon-tweet-count (map #(mc/count @db "tweets" {:user.screen_name "SPIEGELONLINE" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        faz-tweet-count (map #(mc/count @db "tweets" {:user.screen_name "FAZ_NET" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        bild-tweet-count (map #(mc/count @db "tweets" {:user.screen_name "BILD" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        tagesschau-tweet-count (map #(mc/count @db "tweets" {:user.screen_name "tagesschau" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        days-since-start (apply concat (repeat 5 (range days-running)))
        ;; tweet-count-2 (loop [hours-list [] tweet-dist tweet-count] (if (empty? tweet-dist) hours-list (recur (conj hours-list (vec (take 24 tweet-dist))) (drop 24 tweet-dist))))
        ;; avg-tweets-per-hour (map (fn [hour] (/ (reduce + (map (fn [count] (get count hour)) tweet-count-2)) (count tweet-count-2))) (range 24))
        tweet-counts (concat sz-tweet-count spon-tweet-count faz-tweet-count bild-tweet-count tagesschau-tweet-count)
        grouping (apply concat (map #(repeat (count sz-tweet-count) %) ["sz" "spon" "faz" "bild" "tagesschau"]))]
    (view (line-chart days-since-start tweet-counts :legend true :group-by grouping)))


  (let [days-running (t/in-hours (t/interval (t/date-time 2014 7 2) (t/date-time 2014 9 12)))
        dates (take days-running (p/periodic-seq (t/date-time 2014 7 2) (t/hours 1)))
        tweet-count (map #(mc/count @db "tweets" {:created_at {$gte % $lt (t/plus % (t/hours 1))}}) dates)
        time (range days-running)
        tweet-count-2 (loop [hours-list []
                             tweet-dist tweet-count]
                        (if (empty? tweet-dist)
                          hours-list
                          (recur (conj hours-list (vec (take 24 tweet-dist))) (drop 24 tweet-dist))))
        avg-tweets-per-hour (map (fn [hour] (/ (reduce + (map (fn [count] (get count hour)) tweet-count-2)) (count tweet-count-2))) (range 24))]
    (view (line-chart time tweet-count))
    (view (line-chart (range 24) avg-tweets-per-hour)))

  (let [days-running (t/in-days (t/interval (t/date-time 2014 7 2) (t/date-time 2014 9 12)))
        dates (take days-running (p/periodic-seq (t/date-time 2014 7 2) (t/days 1)))
        tweet-count (map #(mc/count @db "tweets" {:created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        time (range days-running)]
    (view (line-chart time tweet-count)))

  (->> (mc/count @db "tweets" {:created_at {$gt (t/date-time 2014 9 18)}})
       aprint)




  )

;; --- TODO ---
;; twitter-nlp
;; html compression
