(ns ceres.core
  (:require [figwheel.client :as fw :include-macros true]
            [weasel.repl :as ws-repl]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ceres.communicator :refer [connect!]]
            [cljs.core.async :refer [put! chan <! alts! >!]])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]
                   [cljs.core.async.macros :refer [go go-loop]] ))

(enable-console-print!)

(def uri (goog.Uri. js/document.URL))

(def ssl? (= (.getScheme uri) "https"))

(println "Greetings commander")

;; fire up repl
#_(do
    (ns weasel.startup)
    (require 'weasel.repl.websocket)
    (cemerick.piggieback/cljs-repl
        :repl-env (weasel.repl.websocket/repl-env
                   :ip "0.0.0.0" :port 17782)))

#_(ws-repl/connect "ws://localhost:17782" :verbose true)

(def app-state
  (atom
   {:tweets
    []}))

#_(fw/watch-and-reload
  ;; :websocket-url "ws://localhost:3449/figwheel-ws" default
 :jsload-callback (fn [] (print "reloaded"))) ;; optional callback



(defsnippet tweet-item "templates/main.html" [:.tweet-item]
  [tweet owner]
  {[:.tweet-text] (content (:text tweet))
   [:.tweet-author] (content (str "@" (:author tweet)))
   [:.tweet-timestamp] (content (:timestamp tweet))
   [:.tweet-reaction] (content
                       (let [reaction (or (:retweet tweet) (:reply tweet))]
                         (if reaction
                           (html [:span.glyphicon.glyphicon-retweet])
                           "")))
   [:.tweet-url] (if (:url tweet)
                   (do-> (set-attr "href" (:url tweet))
                         (content (:url tweet)))
                   (content ""))})



(deftemplate main-view "templates/main.html" [data owner]
  {[:#tweet-collection] (content (doall (map #(tweet-item % owner) data)))})


(defn tweets-view
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:ws-in (chan)
       :incoming (chan)
       :ws-out (chan)})

    om/IWillMount
    (will-mount [_]
      (go
        (let [connection (<! (connect!
                              (str
                               (if ssl?  "wss://" "ws://")
                               (.getDomain uri)
                               ":" (.getPort uri)
                               "/tweets/ws")))]
            (om/set-state! owner :ws-in (:in connection))
            (om/set-state! owner :ws-out (:out connection))
            (>! (:in connection) {:topic :greeting :data ""})
            (loop []
              (let [new-tweet (<! (:out connection))]
                (om/transact!
                 app
                 :tweets
                 (fn [tweets]
                   (if (:recent-tweets new-tweet)
                     (:recent-tweets new-tweet)
                     (vec (take 100 (into [new-tweet] tweets))))))
                (recur))))))

    om/IRenderState
    (render-state [this {:keys [ws-out] :as state}]
      (om/build main-view (:tweets app) {:init-state state}))))


(om/root
 tweets-view
 app-state
 {:target (. js/document (getElementById "main-container"))})
