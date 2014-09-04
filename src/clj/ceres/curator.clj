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
            [clj-time.coerce :as c]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :refer [sh]]
            [opennlp.nlp :refer [make-tokenizer make-detokenizer]]
            [ceres.collector :refer [db custom-formatter news-accounts store]])
 (:import org.bson.types.ObjectId))

(timbre/refer-timbre)

(defrecord Source [name articles])
(defrecord Article [source tweet reactions])
(defrecord Reaction [tweet reactions])


#_(def tokenize (make-tokenizer "/home/konny/data/open-nlp/de-token.bin"))

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


(defn compute-diffusion
  "Compute the distribution of news account mentions"
  [user]
  (->> (mc/count @db "tweets" {$and [{$or [{"entities.user_mentions.screen_name" user}
                                          {"retweeted_status.user.screen_name" user}
                                          {"in_reply_to_screen_name" user}]}
                                                    {:created_at {$gt (t/date-time 2014 7 1)}}]})))


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


(defn get-news-diffusion []
  (mapv #(vec [% (compute-diffusion %)]) news-accounts))


(defn get-articles-count
  "Compute articles count"
  []
  (mc/count @db "articles"))


(defn find-source
  "Find the source of a given article"
  [id]
  (mc/find-maps @db "urls" {$and [{:article id} {:source {$ne nil}}]} [:source :tweet :ts]))


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


(defn compute-dfs
  "Compute the amount of elements in the impact tree"
  [tree]
  (loop [counter 0
         loc tree]
    (if (zip/end? loc)
      counter
      (recur
       (if (nil? (zip/node loc))
         counter
         (inc counter))
       (zip/next loc)))))


(defn simplify-tree
  "Shows only specific elements in the nodes of an impact graph"
  [tree]
  (loop [pub-time (-> (zip/root tree) :source :ts)
         hashtags []
       ;;  tokens []
         loc tree]
    (if (zip/end? loc)
      {:root (zip/root loc)
       ;; :tokens nil (frequencies tokens)
       :hashtags (frequencies (map :text hashtags))}
      (recur
       pub-time
       (if-let [node (zip/node loc)]
         (->> node :tweet :entities :hashtags (into hashtags))
         hashtags)
       #_(if (zip/node loc)
         (conj tokens (clojure.set/difference (into #{} (-> loc zip/node :tweet :text tokenize)) (first tokens)))
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

(defn compute-impact-forest [source]
  (let [source-origins (mc/find-maps db "origins" {:source source})]
    (vec (pmap compute-impact-tree source-origins))))


(defn example-tree []
  (let [tree (compute-impact-tree (mc/find-map-by-id @db "origins" (ObjectId. "53e4a2f8e4b04c026f6f5a3d")))] ; alternativ "53de3d68657a74caed255892" "53d7a7ad657ad4126658d0ba" "53de089f657a439ad20d6133"
    {:graph (simplify-tree tree)
     :height (tree-height tree)
     :size (compute-dfs tree)}))


(defn random-tree []
  (let [tree (-> (mc/find-maps @db "origins" {:source {$in news-accounts}}) rand-nth compute-impact-tree)]
    {:graph (simplify-tree tree)
     :height (tree-height tree)
     :size (compute-dfs tree)}))


(defn analyze-tree [tree]
  (loop [counter 0
         max-path 0
         users []
         hashtags []
         delays []
         loc tree]
    (if (zip/end? loc)
      {:size counter
       :height max-path
       :users users
       :hashtags (vec (into #{} (map :text hashtags)))
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
             (conj users (-> {:name (:screen_name user) :followers (:followers_count user)})))
         users)
       (if-let [node (zip/node loc)]
         (->> node :tweet :entities :hashtags (into hashtags))
         hashtags)
       (if (zip/node loc)
         (let [pub-time (-> (zip/root tree) :source :ts)
               post-delay (if (t/after? (-> loc zip/node :tweet :created_at) pub-time)
                            (t/interval pub-time (-> loc zip/node :tweet :created_at))
                            (t/interval (-> loc zip/node :tweet :created_at) pub-time))]
           (conj delays (t/in-hours post-delay)))
         delays)
       (zip/next loc)))))


(defn compute-summary [source]
  (let [impact-forest (compute-impact-forest source)
        analytics (map analyze-tree impact-forest)
        sizes (map :size analytics)
        heights (map :height analytics)
        users (frequencies (flatten (map :users analytics)))
        hashtags (frequencies (flatten (map :hashtags analytics)))
        overall-size (reduce + sizes)]
    {:source source
     :articles (count impact-forest)
     :top-users (take 50 (sort-by second > users))
     :hastags (take 50 (sort-by second > hashtags))
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
   (doseq [x (mc/find-maps db "tweets")]
     (mc/update-by-id
      db
      "tweets"
      (:_id x)
      (update-in x [:created_at] #(f/parse custom-formatter (:created_at %))) )))

  custom-formatter

  (-> (example-tree) pprint)


  ;; "53e4a2f8e4b04c026f6f5a3d"

  (pprint (map compute-summary news-accounts))


  (let [tree (-> (mc/find-maps db "origins" {:source {$in news-accounts}}) rand-nth compute-impact-tree)
        ds (loop [pub-time (-> (zip/root tree) :source :ts)
                  dates []
                 loc tree]
            (if (zip/end? loc)
              dates
              (recur
               pub-time
               (if (zip/node loc)
                 (let [post-delay (if (t/after? (-> loc zip/node :tweet :created_at) pub-time)
                                    (t/interval pub-time (-> loc zip/node :tweet :created_at))
                                    (t/interval (-> loc zip/node :tweet :created_at) pub-time))]
                   (conj dates (t/in-hours post-delay)))
                 dates)
               (zip/next loc))))]
    ds)

  (mc/count db "origins" {:source "SPIEGELONLINE"})

  (-> (mc/find-maps db "origins" {:source {$in news-accounts}})
      rand-nth
      compute-impact-tree
      analyze-tree
      pprint)

  (-> (mc/find-maps @db "tweets" {:created_at {$gt (t/today)}})
      count
      time)

  ;; twitter-nlp
  ;; html compression


)
