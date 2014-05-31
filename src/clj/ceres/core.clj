(ns ceres.core
  (:gen-class :main true)
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [ceres.curator :refer [store]]
            [gezwitscher.core :refer [start-filter-stream]]
            [ceres.assembler :refer [page detail]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [put! timeout sub chan <!! >!! <! >! go go-loop] :as async]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)
(timbre/set-config! [:appenders :spit :enabled?] true)
(timbre/set-config! [:shared-appender-config :spit-filename] "resources/collector.log")


(defn extract-tweet-data [tweet]
  "Prepares data for client"
  {:timestamp (:created_at tweet)
   :author (-> tweet :user :screen_name)
   :text (:text tweet)
   :reply (:in_reply_to_status_id_str tweet)
   :retweet (if (:retweeted_status tweet)
              (-> tweet :retweeted_status :id)
              nil)})


(defn twitter-handler [tweet]
  "Stores incoming tweets and notifies clients"
  (do
    (info (str "Storing tweet") (:id tweet))
    (store tweet)
    (put! (:out-chan @twitter-state) (extract-tweet-data tweet))
    (swap! twitter-state update-in [:recent-tweets] (fn [old new] (vec (take 100 (into [new] old)))) tweet)))


(def twitter-state
  (atom
   {:credentials {:consumer-key (or (System/getenv "TWITTER_API_KEY") "****")
                  :consumer-secret (or (System/getenv "TWITTER_API_SECRET") "****")
                  :access-token (or (System/getenv "TWITTER_ACCESS_TOKEN") "****")
                  :access-token-secret (or (System/getenv "TWITTER_ACCESS_TOKEN_SECRET") "****")}
    :handler  twitter-handler
    :recent-tweets []
    :out-chan (chan)
    :follow [114508061 18016521 5734902 40227292 2834511]
    :track ["@FAZ_NET" "@tagesschau" "@dpa" "@SZ" "@SPIEGELONLINE"]}))


(defn dispatch [{:keys [topic data]}]
  "Dispatch incoming websocket-requests"
  (case topic
    :greeting {:recent-tweets (mapv extract-tweet-data (:recent-tweets @twitter-state))}))


(defn tweet-handler [request]
  (with-channel request channel
    (go-loop [m (<! (:out-chan @twitter-state))]
      (when m
        (debug "sending msg:" (pr-str m))
        (send! channel (pr-str m))
        (recur (<! (:out-chan @twitter-state)))))
    (on-close channel
              (fn [status]
                (swap! twitter-state assoc-in [:out-chan] (chan))
                (info "tweet channel closed: " status)))
    (on-receive channel
                (fn [data]
                  (info (str "receiving msg: " (java.util.Date.)))
                  (send! channel (str (dispatch (read-string data))))))))


(defroutes all-routes
  (resources "/")
  (GET "/tweets/ws" [] tweet-handler)
  (GET "/*" [] (io/resource "public/index.html")))


(defn -main [& args]
  (info "Starting twitter collector...")
  (run-server (site #'all-routes) {:port 8082 :join? false})
  (start-filter-stream @twitter-state))


(comment

  (def stop-stream (start-filter-stream @twitter-state))

  (stop-stream)

  (def server (run-server (site #'all-routes) {:port 8081 :join? false}))

  (server)

  )
