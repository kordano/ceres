(ns ceres.core
  (:require [figwheel.client :as fw :include-macros true]
            [weasel.repl :as ws-repl]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html add-class remove-class]]
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

(def news-sources #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ" "BILD" "DerWesten" "ntvde" "tazgezwitscher" "welt" "ZDFheute" "N24_de"})

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
    :news-diffusion nil
    :tweet-count 0}))

#_(fw/watch-and-reload
  ;; :websocket-url "ws://localhost:3449/figwheel-ws" default
 :jsload-callback (fn [] (print "reloaded"))) ;; optional callback


;; --- D3 barchart ---

(defn x
  "Compute ordinal x component"
  [data width]
  (-> d3
      .-scale
      (.ordinal)
      (.domain (vec (keys data)))
      (.rangeRoundBands [0 width] 0.5)))


(defn y
  "Compute linear y component"
  [data height]
  (-> d3
      .-scale
      (.linear)
      (.domain [0 (apply max (vals data))])
      (.range [0 height])))


(defn create-x-axis [x]
  (-> d3
      .-svg
      (.axis)
      (.scale x)
      (.orient "bottom")))


(defn create-y-axis [y]
  (-> d3
      .-svg
      (.axis)
      (.scale y)
      (.orient "left")))

(defn create-remove-svg
  "Create remove function"
  [target]
  (-> d3
      (.select (str "#" target))
      (.select "svg")
      (.remove)))

(defn create-svg
  "Create svg function"
  [target margin width height]
  (let [target-svg (-> d3
                       (.select (str "#" target))
                       (.select "svg"))]
    (do
      (if (nil? target-svg)
        (.log js/console "empty")
        (-> target-svg
            (.remove)))
      (-> d3
          (.select (str "#" target))
          (.append "svg")
          (.attr {:width  (+ width (margin :left) )
                  :height (+ height (margin :top) (margin :bottom))
                  :id (str target "-chart")})
          (.append "g")
          (.attr {:transform (str "translate(" (margin :left) "," (margin :top) ")")})))))


(defn create-bars
  "Create histogram bars"
  [target data height width margin]
  (let [svg (create-svg target margin width height)
        x1 (x data width)
        y1 (y data height)
        x-axis (create-x-axis x1)
        y-axis (create-y-axis y1)]
    (do
      (-> svg
          (.append "g")
          (.attr {:class "x axis"
                  :transform (str "translate(0," height ")")})
          (.call x-axis))
      (-> svg
          (.selectAll "g.bar")
          (.data data)
          (.enter)
          (.append "g")
          (.attr (clj->js {:class "bar"
                           :transform (fn [[k v] i] (str "translate(" (x1 k) ",0)"))}))
          (.style {:fill "steelblue"})))))


(defn draw-bars
  "Draw the bar charts in given target node with given data"
  [target data height width margin]
  (let [bars (create-bars target data height width margin)
        x1 (x data width)
        y1 (y data height)]
    (do
      (-> bars
          (.append "rect")
          (.attr {:height 0
                  :y height
                  :width (.rangeBand x1)})
          (.transition)
          (.delay 200)
          (.duration 500)
          (.attr {:y (fn [[k v] i] (- height  (y1 v)))
                  :height (fn [[k v] i] (y1 v))}))
      (-> bars
          (.append "text")
          (.attr {:x (/ (.rangeBand x1) 2)
                  :y (fn [[k v] i] (- height (- (y1 v) 15)))
                  :text-anchor "middle"})
          (.style "fill" "white")
          (.text (fn [[k v] i] v))))))


;; --- D3 line chart ---

(defn line-x [data width]
  (-> d3
      .-time
      (.scale)
      (.domain (-> d3
                   (.extent data (fn [d] (:date d)))))
      (.range [0 width])))

(defn line-y [data height]
  (-> d3
      .-scale
      (.linear)
      (.domain (-> d3
                   (.extent data (fn [d] (:count d)))))
      (.range [height 0])))

(defn line []
  (-> d3
      .-svg
      (.line)
      (.x (fn [d i] (:date d)))
      (:y (fn [d i] (:count d)))))

(defn create-lines [target data height width margin]
  (let [svg (create-svg target margin width height)
        d-line (line)]
    (-> svg
        (.append "path")
        (.datum data)
        (.attr {:class "line"
                :d d-line}))))



;; --- tweet-list templates ---

(defsnippet tweet-item "templates/tweet-list.html" [:.tweet-item]
  [tweet owner]
  {[:.tweet-item] (if (news-sources (:author tweet))
                    (add-class "tweet-news-item")
                    (remove-class "fade"))
   [:.tweet-text] (content (:text tweet))
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


(deftemplate tweet-list "templates/tweet-list.html" [data owner]
  {[:#tweet-collection] (content (doall (map #(tweet-item % owner) (:tweets data))))
   [:#tweet-overall-count] (content (:tweet-count data))})

;; --- tweet-list templates end ---


(deftemplate navbar "templates/navbar.html" [app]
  {[:#ceres-brand] (content "Collector")
   [:#tweet-list-link] (listen :on-click #(.log js/console "tweet list"))
   [:#statistics-link] (listen :on-click #(.log js/console "statistics"))})


(deftemplate stat-screen "templates/stats.html" [app]
  {[:.chart-selector] (listen :on-click #(.log js/console (.-id (.-target %))))
   [:#diffusion] (listen :on-click #(let [data (:news-diffusion @app)
                                          margin {:top 50 :right 60 :bottom 50 :left 1}
                                          width (- (.-clientWidth (. js/document (getElementById "diffusion-container"))) (margin :left) (margin :right))
                                          height (- 500  (margin :top) (margin :bottom))]
                                      (draw-bars "diffusion-container" data height width margin)))

   [:#tweets-count] (listen :on-click #(let [data (:news-frequencies @app)
                                           margin {:top 50 :right 60 :bottom 50 :left 1}
                                           width (- (.-clientWidth (. js/document (getElementById "tweets-count-container"))) (margin :left) (margin :right))
                                          height (- 500  (margin :top) (margin :bottom))]
                                         (draw-bars "tweets-count-container" data height width margin)))})


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
                               8082 #_(.getPort uri)
                               "/tweets/ws")))]
            (om/set-state! owner :ws-in (:in connection))
            (om/set-state! owner :ws-out (:out connection))
            (>! (:in connection) {:topic :greeting :data ""})
            (>! (:in connection) {:topic :news-frequencies :data ""})
            (>! (:in connection) {:topic :news-diffusion :data ""})
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

                  :news-diffusion
                  (om/transact!
                   app
                   :news-diffusion
                   (fn [old] data))


                  :news-frequencies
                  (do
                    (om/transact!
                     app
                     :news-frequencies
                     (fn [old] data))
                    (let [margin {:top 50 :right 60 :bottom 50 :left 1}
                          width (- (.-clientWidth (. js/document (getElementById "tweets-count-container"))) (margin :left) (margin :right))
                          height (- 500  (margin :top) (margin :bottom))]
                      (draw-bars "tweets-count-container" data height width margin))))
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


(comment
  (go
    (let [connection (<! (connect!
                          (str
                           (if ssl?  "wss://" "ws://")
                           (.getDomain uri)
                           ":"
                           8082 #_(.getPort uri)
                           "/tweets/ws")))]
      (>! (:in connection) {:topic :time-distribution :data [5 6]})
      (println (-> (<! (:out connection)) :data ffirst :date js/Date.))))

  )
