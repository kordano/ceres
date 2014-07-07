(ns ceres.core
  (:gen-class :main true)
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [net.cgrand.enlive-html :refer [deftemplate set-attr append html substitute content]]
            [ceres.curator :refer [store get-tweet-count export-edn get-news-diffusion get-news-frequencies get-month-distribution] :as curator]
            [gezwitscher.core :refer [start-filter-stream]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [close! put! timeout sub chan <!! >!! <! >! go go-loop] :as async]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)
#_(timbre/set-config! [:appenders :spit :enabled?] true)
#_(timbre/set-config! [:shared-appender-config :spit-filename] "resources/collector.log")


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


(def server-state (atom nil))


(defn dispatch
  "Dispatch incoming websocket-requests"
  [{:keys [topic data] :as package}]
  (case topic
    :news-frequencies (assoc package :data (get-news-frequencies))
    :news-diffusion (assoc package :data (get-news-diffusion))
    :init (assoc package :data
                 {:recent-tweets (mapv extract-tweet-data (-> @server-state :app :recent-tweets))
                  :tweet-count (get-tweet-count)})))

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
  "Reacts to incoming tweets"
  [state tweet]
  (do
    (info (str "Storing tweet") (:id tweet))
    (store tweet)
    (let [data (extract-tweet-data tweet)]
      (doall
       (map #(put! % {:topic :new-tweet :data data}) (-> @state :app :out-chans))))
    (swap!
     state
     update-in
     [:app :recent-tweets]
     (fn [old new] (vec (take 100 (into [new] old))))
     tweet)))

(defn initialize
  "Initializes the server state using a given config file"
  [state path]
  (reset!
   state
   (-> path slurp read-string
       (assoc-in [:app :handler] (partial stream-handler state))
       (assoc-in [:app :out-chans] [])
       (assoc-in [:app :recent-tweets] []))))

(defn handle-export [query]
  (let [[m d] (mapv read-string (clojure.string/split (get query "date") #"-" ))]
       (->> (curator/get-tweets-from-date m d)
           (map str)
           (clojure.string/join "\n"))))

(defroutes all-routes
  (resources "/")
  (GET "/tweets/ws" [] tweet-handler)

  (GET "/tweets/export" request (let [query-params (-> request :query-params)]
                                  (handle-export query-params)))

  (GET "/*" [] (if (= (:build @server-state) :prod)
                 (static-page)
                 (io/resource "public/index.html"))))


(defn -main [& args]
  (initialize server-state (first args))
  (info "Starting twitter collector...")
  (info (clojure.pprint/pprint @server-state))
  (run-server (site #'all-routes) {:port (:port @server-state) :join? false})
  (let [{:keys [follow track handler credentials]} (:app @server-state)]
    (start-filter-stream follow track handler credentials)))


(comment

  (initialize server-state "resources/server-config.edn")

  (def stop-stream
    (let [{:keys [follow track handler credentials]} (:app @server-state)]
      (start-filter-stream follow track handler credentials)))

  (stop-stream)

  (def server (run-server (site #'all-routes) {:port (:port @server-state) :join? false}))

  (server)

  )
