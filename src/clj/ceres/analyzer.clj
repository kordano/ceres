(ns ceres.analyzer
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
            [opennlp.nlp :refer [make-tokenizer make-detokenizer]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [mean variance quantile]]
            [incanter.charts :as charts]
            [loom.graph :as lg]
            [loom.io :as lio]
            [ceres.collector :refer [db custom-formatter news-accounts store]])
 (:import org.bson.types.ObjectId))

(def time-interval {$gt (t/date-time 2014 8 1) $lt (t/date-time 2014 9 1)})

(defrecord Publication [source reactions])

(defn short-metrics [coll]
  {:mean (mean coll)
   :std (Math/sqrt (variance coll))
   :quantiles (quantile coll)})


(defn find-reactions [pid]
  (let [reactions (mc/find-maps @db "reactions" {:source pid})]
    (Publication. pid (vec (pmap #(find-reactions (:publication %)) reactions)))))


(defn reaction-tree [pub]
  (zip/zipper
   (fn [node] true)
   (fn [node] (:reactions node))
   (fn [node new-children] (assoc-in node [:reactions] new-children))
   (find-reactions pub)))


(defn find-full-reactions
  "extended reaction tree recursion"
  [pid]
  (let [publication (mc/find-map-by-id @db "publications" pid)
        reactions (mc/find-maps @db "reactions" {:source pid})]
    (Publication. publication (vec (pmap #(find-full-reactions (:publication %)) reactions)))))


(defn full-reaction-tree [pub]
  (zip/zipper
   (fn [node] true)
   (fn [node] (:reactions node))
   (fn [node new-children] (assoc-in node [:reactions] new-children))
   (find-full-reactions pub)))


(defn summary [tree]
  (loop [size 0
         max-path 0
         loc tree]
    (if (zip/end? loc)
      {:size size
       :source (-> (zip/root tree) :source)
       :height max-path}
      (recur
       (if (zip/node loc) (inc size) size)
       (if (zip/node loc) (-> loc zip/path count (max max-path)) max-path)
       (zip/next loc)))))


(defn analyze-delays
  "Create tree analyzing delay times relativ to first post time"
  [tree]
  (loop [delays []
         loc tree]
    (if (zip/end? loc)
      {:source (-> (zip/root tree) :source :_id)
       :delays delays}
      (recur
       (if (zip/node loc)
         (let [pub-time (-> (zip/root tree) :source :ts)
               post-delay (if (t/after? (-> loc zip/node :source :ts) pub-time)
                            (t/interval pub-time (-> loc zip/node :source :ts))
                            (t/interval (-> loc zip/node :source :ts) pub-time))]
           (conj delays (t/in-seconds post-delay)))
         delays)
       (zip/next loc)))))




(defn hashtags-of-the-day [date]
  (let [pubs (mc/find-maps @db "publications" {:ts {$gt date
                                                    $lt (t/plus date (t/days 1))}})]
    (->> pubs
         (map :hashtags)
         flatten
         (remove nil?)
         (pmap #(mc/find-map-by-id @db "hashtags" %))
         (pmap :text)
         frequencies
         (sort-by second >)
         (take 25))))


(defn users-of-the-day
  "Get user with most posts of given date"
  [date]
  (let [pubs (mc/find-maps @db "publications" {:ts {$gt date
                                                    $lt (t/plus date (t/days 1))}})]
    [(count pubs)
     (->> pubs
          (map :user)
          frequencies
          (sort-by second >)
          (take 25)
          (map (fn [[k v]] [(:screen_name (mc/find-map-by-id @db "users" k))
                           v])))]))

(defn create-d3-graph
  "Converts zipper into d3 readable format"
  [tree]
  (loop [counter 0
         types {}
         nodes []
         texts {}
         links []
         loc tree]
    (if (zip/end? loc)
      {:nodes nodes
       :types types
       :texts texts
       :links links}
      (if-let [node (zip/node loc)]
        (let [status-text (->> node :source :tweet (mc/find-map-by-id @db "tweets") :text str)
              id (-> node :source :_id str)]
          (recur
           (inc counter)
           (assoc types id (-> node :source :type))
           (vec (conj nodes id))
           (assoc texts id status-text)
           (if-not (= node (zip/root tree))
             (conj links {:source counter :target (.indexOf nodes (-> loc zip/up zip/node :source :_id str))})
             links)
           (zip/next loc)))
        (recur counter types nodes texts links (zip/next loc))))))


(comment

  ;; hashtag distribution of one-time-posters
  (let [user-freq (->> (mc/find-maps @db "publications")
                       (map :user)
                       frequencies)]
    (->> user-freq
         (remove (fn [[k v]] (> v 1)))
         keys
         (map #(mc/find-one-as-map @db "publications" {:user %}))
         (map :hashtags)
         flatten
         (remove nil?)
         frequencies
         (sort-by second >)
         (take 25)
         (map (fn [[k v]] [(:text (mc/find-map-by-id @db "hashtags" k))
                          v]))
         aprint
         time))


  ;; hashtag distribution
  (->> (mc/find-maps @db "publications")
       (map :hashtags)
       (remove nil?)
       flatten
       frequencies
       (sort-by second >)
       (take 25)
       (map (fn [[k v]] [(:text (mc/find-map-by-id @db "hashtags" k))
                        v]))
       aprint
       time)


  (let [users (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}}))]
    (->> (mc/find-maps @db "publications" {:user {$in users}})
         (pmap (comp analyze-delays full-reaction-tree :_id))
         (pmap :delays)
         flatten
         frequencies
         (sort-by first >)
         time))

  (def all-delays
    (let [users (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}}))]
      (->> (mc/find-maps @db "publications" {:user {$in users}
                                             :ts {$lt (t/date-time 2014 9 1)}})
           (pmap (comp :delays analyze-delays full-reaction-tree :_id)))))



  (letfn [(dispatch-types [type]
            (case type
              "source" 0
              "retweet" 1
              "reply" 2
              "share" 3
              4))]
    (let [source-uids (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}}))
          graphs (->> (mc/find-maps @db "publications" {:user {$in source-uids}})
                      (pmap (comp summary reaction-tree :_id))
                      (sort-by :size >)
                      (take 50)
                      (pmap (comp create-d3-graph full-reaction-tree :source)))
          cleaned-graphs
          (pmap
           (fn [graph]
             (let [types (:types graph)
                   texts (:texts graph)]
               (update-in
                (dissoc graph :types)
                [:nodes]
                #(mapv
                  (fn [k]
                    {:name k
                     :value (get texts k)
                     :group (dispatch-types (get types k))})
                  %))))
           graphs)]
      (time
       (loop [gs cleaned-graphs
              counter 0]
         (when-not (empty? gs)
           (with-open [w (clojure.java.io/writer (str "data/graph-" counter ".edn"))]
             (binding [*print-length* false
                       *out* w]
               (pr (first gs))))
           (recur
            (rest gs)
            (inc counter)))))))


  ;; find all related shares
  (let [origins (mc/find-maps @db "origins" {:source nil
                                             :ts {$gt (t/date-time 2014 8 1)
                                                  $lt (t/date-time 2014 9 1)}})]
    (->> origins
         (map (comp :in_reply_to_status_id #(mc/find-map-by-id @db "tweets" (:tweet %))))
         (take 10)
         aprint))


  (->> (mc/find-maps @db "publications" {:type :share})
       count
       aprint
       time)

  ((comp float /) (mc/count @db "reactions" {:source nil})
     (mc/count @db "reactions"))


  ;; asdasd

  (mc/count @db "origins" {:source nil
                               :ts time-interval })

  (->> (mc/count @db "tweets" {:created_at time-interval
                               :user.screen_name {$nin news-accounts}
                               :retweeted_status.id_str nil
                               :in_reply_to_status_id_str nil})
       aprint
       time)

  (->> (mc/find-maps @db "origins" {:source nil
                                    :article {$ne nil}
                                    :ts time-interval})
       (pmap (fn [{:keys [tweet article]}] [article (mc/find-map-by-id @db "tweets" tweet)]))
       (pmap (fn [[article {:keys [in_reply_to_status_id retweeted_status] :as tweet}]]
               (if (or in_reply_to_status_id retweeted_status)
                 :no-share
                 [article tweet])))
       (remove #{:no-share})
       (take 10)
       (map first)
       (pmap #(mc/find-map-by-id @db "articles" %))
       (map :url)
       (map #(mc/find-one-as-map @db "urls" {:url %}))
       aprint)


  (mc/count @db "urls")

  (mc/count @db "articles" {:ts time-interval})


  (->> (mc/find-maps @db "articles" {:ts time-interval})
       (pmap (comp (fn [{:keys [user]}] (:screen_name user)) #(mc/find-one-as-map @db "tweets" {:created_at %}) :ts))
       (into #{}))


  (mc/count @db "tweets" {:created_at time-interval
                          :user.screen_name {$in news-accounts}})

  (mc/count @db "tweets" {:created_at time-interval
                          :user.screen_name {$nin news-accounts}
                          :in_reply_to_status_id {$ne nil}
                          :retweeted_status nil})

  (mc/count @db "publications" {:type :reply})

  )
