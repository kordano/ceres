(ns ceres.core
  (:gen-class :main true)
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [net.cgrand.enlive-html :refer [deftemplate set-attr append html substitute content]]
            [ceres.curator :refer [store get-tweet-count export-edn get-news-diffusion get-news-frequencies get-month-distribution]]
            [gezwitscher.core :refer [start-filter-stream]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [close! put! timeout sub chan <!! >!! <! >! go go-loop] :as async]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)
(timbre/set-config! [:appenders :spit :enabled?] true)
(timbre/set-config! [:shared-appender-config :spit-filename] "resources/collector.log")


(deftemplate static-page
  (io/resource "public/index.html")
  []
  [:#bootstrap-css] (set-attr "href" "static/bootstrap/bootstrap-3.1.1-dist/css/bootstrap.min.css")
  [:#bootstrap-theme-css] (set-attr "href" "static/bootstrap/bootstrap-3.1.1-dist/css/bootstrap-theme.min.css")
  [:#react-js] (set-attr "src" "static/react/react-0.9.0.min.js")
  [:#jquery-js] (set-attr "src" "static/jquery/jquery-1.11.0.min.js")
  [:#bootstrap-js] (set-attr "src" "static/bootstrap/bootstrap-3.1.1-dist/js/bootstrap.min.js")
  [:#d3-js] (set-attr "src" "static/d3/d3.min.js")
  [:#js-files] (substitute (html [:script {:src "js/main.js" :type "text/javascript"}])))


(defn extract-tweet-data
  "Prepares data for client"
  [tweet]
  {:timestamp (:created_at tweet)
   :author (-> tweet :user :screen_name)
   :text (:text tweet)
   :reply (:in_reply_to_status_id_str tweet)
   :url (let [urls (-> tweet :entities :urls)]
          (if (empty? urls)
            nil
            (-> urls first :url)))
   :retweet (if (:retweeted_status tweet)
              (-> tweet :retweeted_status :id)
              nil)})


(def server-state
  (atom
   {:build "dev"
    :twitter
    {:credentials {:consumer-key (or (System/getenv "TWITTER_API_KEY") "****")
                   :consumer-secret (or (System/getenv "TWITTER_API_SECRET") "****")
                   :access-token (or (System/getenv "TWITTER_ACCESS_TOKEN") "****")
                   :access-token-secret (or (System/getenv "TWITTER_ACCESS_TOKEN_SECRET") "****")}
     :handler  (fn [tweet]
                 (do
                   (info (str "Storing tweet") (:id tweet))
                   (store tweet)
                   (let [data (extract-tweet-data tweet)]
                     (doall
                      (map #(put! % {:topic :new-tweet :data data}) (-> @server-state :twitter :out-chans))))
                   (swap!
                    server-state
                    update-in
                    [:twitter :recent-tweets]
                    (fn [old new] (vec (take 100 (into [new] old))))
                    tweet)))
     :recent-tweets []
     :out-chans []
     :follow [114508061 18016521 5734902 40227292 2834511]
     :track ["@FAZ_NET" "@tagesschau" "@dpa" "@SZ" "@SPIEGELONLINE"]}}))


(defn dispatch
  "Dispatch incoming websocket-requests"
  [{:keys [topic data]}]
  (case topic
    :news-frequencies {:topic :news-frequencies :data (get-news-frequencies)}
    :time-distribution {:topic :time-distribution :data (mapv get-month-distribution data)}
    :news-diffusion {:topic :news-diffusion :data (get-news-diffusion)}
    :greeting {:topic :new-tweet
               :data {:recent-tweets (mapv extract-tweet-data (-> @server-state :twitter :recent-tweets))
                                  :tweet-count (get-tweet-count)}}))


(defn tweet-handler
  "Handle incoming tweets"
  [request]
  (let [out-chan (chan)]
    (with-channel request channel
      (swap! server-state update-in [:twitter :out-chans] conj out-chan)
      (go-loop [m (<! out-chan)]
        (when m
          (send! channel (pr-str m))
          (recur (<! out-chan))))
      (on-close channel
                (fn [status]
                  (swap! server-state update-in [:twitter :out-chans] (fn [old new] (vec (remove #(= new %) old))) out-chan)
                  (close! out-chan)
                  (info "tweet channel closed: " status "!")))
      (on-receive channel
                  (fn [data]
                    (info "tweet channel opened!")
                    (send! channel (str (dispatch (read-string data)))))))))


(defroutes all-routes
  (resources "/")
  (GET "/tweets/ws" [] tweet-handler)
  (GET "tweets/export" [] (export-edn))
  (GET "/*" [] (if (= (:build @server-state) "prod")
                 (static-page)
                 (io/resource "public/index.html"))))


(defn -main [& args]
  (let [[port build] args]
    (swap! server-state assoc :build build)
    (info "Starting twitter collector...")
    (info (clojure.pprint/pprint @server-state))
    (run-server (site #'all-routes) {:port (Integer/parseInt port) :join? false})
    (let [{:keys [follow track handler credentials]} (:twitter @server-state)]
      (start-filter-stream follow track handler credentials))))


(comment

  (def stop-stream
    (let [{:keys [follow track handler credentials]} (:twitter @server-state)]
      (start-filter-stream follow track handler credentials)))

  (stop-stream)

  (def server (run-server (site #'all-routes) {:port 8082 :join? false}))

  (server)

)
