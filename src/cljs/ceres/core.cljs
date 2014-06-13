(ns ceres.core
  (:require [figwheel.client :as fw :include-macros true]
            [weasel.repl :as ws-repl]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ceres.communicator :refer [connect!]]
            [strokes :refer [d3]]
            [cljs.core.async :refer [put! chan <! alts! >!]])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]
                   [cljs.core.async.macros :refer [go go-loop]] ))

(enable-console-print!)

(def uri (goog.Uri. js/document.URL))

(def ssl? (= (.getScheme uri) "https"))

(println "Greetings commander")

(strokes/bootstrap)

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
   {:tweets []
    :news-frequencies nil
    :tweet-count 0}))

(fw/watch-and-reload
  ;; :websocket-url "ws://localhost:3449/figwheel-ws" default
 :jsload-callback (fn [] (print "reloaded"))) ;; optional callback


;; --- D3 ---
(defn x [data width]
  (-> d3
      .-scale
      (.ordinal)
      (.domain (vec (range (count data))))
      (.rangeRoundBands [0 width] 0.5)))


(defn y [data height]
  (-> d3
      .-scale
      (.linear)
      (.domain [(apply max data) 0])
      (.range [height 0])))


(defn create-svg [margin width height]
  (-> d3
      (.select "#tweets-count")
      (.append "svg")
      (.attr {:width  (+ width (margin :left) (margin :right))
              :height (+ height (margin :top) (margin :bottom))})
      (.append "g")
      (.attr {:transform (str "translate(" (margin :left) "," (margin :top) ")")})))


(defn create-bars [data height width margin]
  (let [svg (create-svg margin width height)
        x1 (x data width)
        y1 (y data height)]
    (-> svg
        (.selectAll "g.bar")
        (.data data)
        (.enter)
        (.append "g")
        (.attr (clj->js {:class "bar"
                         :transform #(str "translate(" (x1 %2) "," (- height  (y1 (data %2))) ")" )}))
        (.style {:fill "steelblue"}))))


(defn draw-bars [data height width margin]
  (let [bars (create-bars data height width margin)
        x1 (x data width)
        y1 (y data height)]
    (do
      (-> bars
          (.append "rect")
          (.attr {:height  #(y1 %)
                  :width (.rangeBand x1)}))
      (-> bars
          (.append "text")
          (.attr {:x (/ (.rangeBand x1) 2)
                  :y y1
                  :dy "-0.35em"
                  :text-anchor "middle"})
          (.style "fill" "white")
          (.text identity)))))

;; --- D3 end ---
(defsnippet tweet-item "templates/tweet-list.html" [:.tweet-item]
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


(deftemplate navbar "templates/navbar.html" [app]
  {[:#ceres-brand] (content "Collector")
   [:#tweet-list-link] (listen :on-click #(.log js/console "tweet list"))
   [:#statistics-link] (listen :on-click #(.log js/console "statistics"))})


(deftemplate stat-screen "templates/stats.html" [app]
  {[:#selected-stat] (content "Tweets Count")
   [:#tweets-count] (listen
                     :on-mount
                     #(let [data [23 34 99 34 11]
                            margin {:top 50 :right 50 :bottom 50 :left 50}
                            width (- (.-clientWidth (. js/document (getElementById "tweets-count"))) (margin :left) (margin :right))
                            height (- 500  (margin :top) (margin :bottom))]
                        (draw-bars data height width margin)))})


(deftemplate tweet-list "templates/tweet-list.html" [data owner]
  {[:#tweet-collection] (content (doall (map #(tweet-item % owner) (:tweets data))))
   [:#tweet-overall-count] (content (:tweet-count data))})


(defn tweets-view
  "Shows recent tweets, connects to server and updates automatically, "
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:ws-in (chan)
       :ws-out (chan)})

    om/IWillMount
    (will-mount [_]
      (go
        (let [connection (<! (connect!
                              (str
                               (if ssl?  "wss://" "ws://")
                               (.getDomain uri)
                               ":"
                               (.getPort uri)
                               "/tweets/ws")))]
            (om/set-state! owner :ws-in (:in connection))
            (om/set-state! owner :ws-out (:out connection))
            (>! (:in connection) {:topic :greeting :data ""})
            (>! (:in connection) {:topic :news-frequencies :data ""})
            (loop []
              (let [{:keys [topic data] :as package} (<! (:out connection))]
                (case topic
                  :new-tweet
                  (do
                    (om/transact!
                     app
                     :tweets
                     (fn [tweets]
                       (if (:recent-tweets data)
                         (:recent-tweets data)
                         (vec (take 100 (into [data] tweets))))))
                    (om/transact!
                     app
                     :tweet-count
                     (fn [tweet-count]
                       (if (:tweet-count data)
                         (:tweet-count data)
                         (inc tweet-count)))))

                  :news-frequencies
                  (om/transact!
                   app
                   :news-frequencies
                   (fn [old] data)))
                (recur))))))

    om/IRenderState
    (render-state [this {:keys [ws-out] :as state}]
      (om/build tweet-list app {:init-state state}))))


;; --- ROOTS ---

(om/root
 tweets-view
 app-state
 {:target (. js/document (getElementById "side-container"))})

(om/root
 #(om/component (navbar %))
 app-state
 {:target (. js/document (getElementById "main-navbar"))})

(om/root
 #(om/component (stat-screen %))
 app-state
 {:target (. js/document (getElementById "central-container"))})
