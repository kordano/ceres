(ns ceres.core
  (:require [figwheel.client :as fw :include-macros true]
            [weasel.repl :as ws-repl]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! alts >!]])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]
                   [cljs.core.async.macros :refer [go go-loop]] ))

(enable-console-print!)

(println "Greetings commander")

;; fire up repl
#_(do
    (ns weasel.startup)
    (require 'weasel.repl.websocket)
    (cemerick.piggieback/cljs-repl
        :repl-env (weasel.repl.websocket/repl-env
                   :ip "0.0.0.0" :port 17782)))

(ws-repl/connect "ws://localhost:17782" :verbose true)

(def app-state
  (atom
   {:tweets
    [{:user "jane" :text "future here I come" :timestamp "yesterday" :retweet 345 :reply nil}
     {:user "john" :text "no no no and no" :timestamp "three hours ago" :retweet nil :reply 456}
     {:user "adam" :text "work hard!" :timestamp "one hour ago" :retweet nil :reply nil}
     {:user "eve" :text "carpe diem!" :timestamp "now" :retweet 123 :reply nil}]}))

(fw/watch-and-reload
  ;; :websocket-url "ws://localhost:3449/figwheel-ws" default
 :jsload-callback (fn [] (print "reloaded"))) ;; optional callback

(defsnippet tweet-item "templates/main.html" [:.tweet-item]
  [tweet owner]
  {[:.tweet-text] (content (:text tweet))
   [:.tweet-user] (content (:user tweet))
   [:.tweet-timestamp] (content (:timestamp tweet))
   [:.tweet-reaction] (content
                       (let [reaction (or (:retweet tweet) (:reply tweet))]
                         (if reaction
                           (html [:span.glyphicon.glyphicon-retweet])
                           "")))})



(deftemplate main-view "templates/main.html" [data owner]
  {[:#tweet-collection] (content (doall (map #(tweet-item % owner) (reverse data))))})

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
      (let [incoming (om/get-state owner :incoming)]
        (go-loop []
          (let [new-tweet (<! incoming)]
            (om/transact! app :tweets (fn [tweets] (conj tweets new-tweet)))
            (recur))))
      #_(go
          (let [connection (<! (connect! (str "ws://" host "/tweets/ws")))]
            (om/set-state! owner :ws-in (:in connection))
            (om/set-state! owner :ws-out (:out connection)))))

    om/IRenderState
    (render-state [this {:keys [incoming] :as state}]
      (om/build main-view (:tweets app) {:init-state state}))))


(om/root
 tweets-view
 app-state
 {:target (. js/document (getElementById "main-container"))})

#_(swap! app-state update-in [:tweets] conj
       {:user "eve" :text "oh yeah" :timestamp "5 minutes ago" :retweet nil :reply nil})
