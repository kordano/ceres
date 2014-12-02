(ns ceres.core
  (:gen-class :main true)
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [net.cgrand.enlive-html :refer [deftemplate set-attr append html substitute content]]
            [ceres.collector :refer [store store-raw-tweet set-db] :as collector]
            [ceres.curator :refer [get-articles-count] :as curator]
            [ceres.executor :refer [start-executor]]
            [gezwitscher.core :refer [start-filter-stream gezwitscher]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [close! put! timeout sub chan <!! >!! <! >! go go-loop] :as async]
            [taoensso.timbre :refer [info debug error warn] :as timbre]))

(timbre/refer-timbre)

(deftemplate static-page
  (io/resource "public/index.html")
  []
  [:#bootstrap-css] (set-attr "href" "static/bootstrap/darkly/bootstrap.min.css")
  [:#react-js] (set-attr "src" "static/react/react-0.9.0.min.js")
  [:#jquery-js] (set-attr "src" "static/jquery/jquery-1.11.0.min.js")
  [:#bootstrap-js] (set-attr "src" "static/bootstrap/bootstrap-3.1.1-dist/js/bootstrap.min.js")
  [:#d3-js] (set-attr "src" "static/d3/d3.min.js")
  [:#js-files] (substitute (html [:script {:src "js/main.js" :type "text/javascript"}])))


(defn format-article
  "Formats a given article record removing html content and content-type"
  [article]
  (-> (update-in article [:article] #(dissoc % :html :content-type))
      (update-in [:article :_id] str)))

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


(def server-state (atom nil))


(defn dispatch
  "Dispatch incoming websocket-requests"
  [{:keys [topic data] :as package}]
  (case topic
    :tweets-count (assoc package :data (curator/get-tweet-count))
    :init (assoc package :data
                 {:recent-articles (-> @server-state :app :recent-articles)
                  :articles-count (get-articles-count)})))


(defn tweet-handler
  "Handle incoming tweets"
  [request]
  (let [out-chan (chan)]
    (with-channel request channel
      (swap! server-state update-in [:app :out-chans] conj out-chan)
      (go-loop [m (<! out-chan)]
        (when m
          (send! channel (pr-str m))
          (recur (<! out-chan))))
      (on-close channel
                (fn [status]
                  (swap! server-state update-in [:app :out-chans] (fn [old new] (vec (remove #(= new %) old))) out-chan)
                  (close! out-chan)
                  (info "tweet channel closed: " status "!")))
      (on-receive channel
                  (fn [data]
                    (send! channel (str (dispatch (read-string data)))))))))


(defn stream-handler
  "React to incoming tweets"
  [state tweet]
  (let [articles (->> (store tweet)
                      (remove nil?)
                      (mapv format-article))
        data (extract-tweet-data tweet)]
    (debug (str "Storing tweet " (:id tweet)))
    (swap! state update-in [:app :recent-articles] (fn [old new] (vec (take 250 (into new old)))) articles)
    (when (not(empty? articles))
      (doall (map #(put! % {:topic :new-article :data articles}) (-> @state :app :out-chans))))
    (swap! state update-in [:app :recent-tweets] (fn [old new] (vec (take 100 (into [new] old)))) tweet)))


(defn stream-handler-beta
  [state tweet]
  (let [{:keys [text user]} (store-raw-tweet tweet)]))


(defn initialize
  "Initialize the server state using a given config file"
  [state path]
  (do
    (reset!
     state
     (-> path slurp read-string
         (assoc-in [:app :out-chans] [])
         (assoc-in [:app :recent-tweets] [])
         (assoc-in [:app :recent-articles] [])))
    (set-db (-> @state :app :db))))


(defn start-stream [state]
  (let [{:keys [follow track credentials]} (:app @state)
        [in out] (gezwitscher credentials)]
    (>!! in {:topic :start-stream :track track :follow follow})
    (let [output (<!! out)]
      (go-loop [status (<! (:status-ch output))]
        (when status
          (stream-handler-beta state status)
          (recur (<! (:status-ch output))))))
    [in out]))


(defroutes all-routes
  (resources "/")

  (GET "/tweets/ws" [] tweet-handler)

  (GET "/*" [] (if (= (:build @server-state) :prod)
                 (static-page)
                 (io/resource "public/index.html"))))


(defn -main [config-path & args]
  (initialize server-state config-path)
  (timbre/set-config! [:appenders :spit :enabled?] true)
  (timbre/set-config! [:shared-appender-config :spit-filename] (:logfile @server-state))
  (info "Starting twitter collector...")
  (when (:init? @server-state)
    (collector/init-mongo))
  (info @server-state)
  (when (:http-server? @server-state)
    (run-server (site #'all-routes) {:port (:port @server-state) :join? false}))
  (let [{{:keys [follow track credentials]} :app} @server-state]
    (start-filter-stream follow track (partial stream-handler server-state) credentials))
  (when (:backup? @server-state)
    (start-executor (:backup-folder @server-state))))


(comment

  (initialize server-state "opt/server-config-1.edn")

  (def stop-stream
    (let [{{:keys [follow track credentials]} :app} @server-state]
      (start-filter-stream follow track store-raw-tweet credentials)))

  (stop-stream)

  (def g (start-stream server-state))

  (>!! (first g) {:topic :stop-stream})

  (def stop-server
    (do
      (timbre/set-config! [:appenders :spit :enabled?] true)
      (timbre/set-config! [:shared-appender-config :spit-filename] (:logfile @server-state))
      (run-server (site #'all-routes) {:port (:port @server-state) :join? false})))

  (stop-server)

  @server-state

  )
