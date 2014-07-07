(ns ceres.core
  (:require [figwheel.client :as fw :include-macros true]
            [weasel.repl :as ws-repl]
            [kioo.om :refer [content set-attr do-> substitute listen prepend append html add-class remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ceres.communicator :refer [connect!]]
            [strokes :refer [d3]]
            [cljs.core.async :refer [timeout put! chan <! alts! >!]])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]
                   [cljs.core.async.macros :refer [go go-loop]] ))

(enable-console-print!)

(def uri (goog.Uri. js/document.URL))

(def ssl? (= (.getScheme uri) "https"))

(println "Greetings commander")

(def news-sources #{"FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ" "BILD" "DerWesten" "ntvde" "tazgezwitscher" "welt" "ZDFheute" "N24_de" "sternde" "focusonline"})

(strokes/bootstrap)

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
   {:tweets []
    :ws-chs [(chan) (chan)]
    :stats-ch (chan)
    :tweets-ch (chan)
    :news-frequencies nil
    :news-diffusion nil
    :tweet-count 0}))

(fw/watch-and-reload
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

(defn draw-chart
  [data target]
  (let [margin {:top 50 :right 60 :bottom 50 :left 1}
        width (- (.-clientWidth (. js/document (getElementById target)))
                 (margin :left)
                 (margin :right))
        height (- 500  (margin :top) (margin :bottom))]
    (draw-bars target data height width margin)))

(deftemplate chart-view "templates/stats.html" [app]
  {[:.chart-selector] (listen :on-click #(.log js/console (.-id (.-target %))))
   [:#diffusion]
   (listen
    :on-click
    #(if (nil? (:news-diffusion @app))
       (let [[ws-in ws-out] (:ws-chs @app)
            out (:stats-ch @app)]
         (go
           (>! ws-in {:topic :news-diffusion :data ""})
           (let [{:keys [topic data] :as package} (<! out)]
             (when (= topic :news-diffusion)
               (.log js/console (str "diffusion data received:" topic ":" data ))
               (om/transact! app :news-diffusion (fn [old] data))
               (draw-chart data "diffusion-container")))))
       (do
         (.log js/console "using cache")
         (draw-chart (:news-diffusion @app) "diffusion-container"))))

   [:#tweets-count]
   (listen
    :on-click
    #(if (nil? (:news-frequencies @app))
       (let [[ws-in ws-out] (:ws-chs @app)
            out (:stats-ch @app)]
         (go
           (>! ws-in {:topic :news-frequencies :data ""})
           (let [{:keys [topic data] :as package} (<! out)]
             (when (= topic :news-frequencies)
               (do
                 (.log js/console (str "frequencies data received:" topic ":" data))
                 (om/transact! app :news-frequencies (fn [old] data))
                 (draw-chart data "tweets-count-container"))))))
       (do
         (.log js/console "using cache")
         (draw-chart (:news-frequencies @app) "tweets-count-container"))))})


(defn stats-view
  "Charts view showing "
  [app owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (let [[ws-in ws-out] (:ws-chs app)
            out (:stats-ch app)]
        (go
          (>! ws-in {:topic :news-frequencies :data ""})
          (let [{:keys [topic data] :as package} (<! out)]
            (do
              (.log js/console (str "frequencies data received: " topic ":" data))
              (om/transact! app :news-frequencies (fn [old] data))
              (draw-chart data "tweets-count-container"))))))
    om/IRender
    (render [this]
      (chart-view app))))


(deftemplate navbar "templates/navbar.html" [app]
  {[:#ceres-brand] (content "Collector")
   [:#diffusion-btn] (listen
                      :on-click
                      (fn [e]
                        (do
                          (.log js/console "diffusion")
                          (om/root
                           #(om/component (dom/p nil "DIFFUSION!!!"))
                           app
                           {:target (. js/document (getElementById "central-container"))}))))


   [:#stats-btn] (listen
                  :on-click
                  (fn [e]
                    (do
                      (.log js/console "statistics")
                      (om/root
                       stats-view
                       app
                       {:target (. js/document (getElementById "central-container"))}))))})


(defn tweets-view
  "Shows recent tweets, connects to server and updates automatically"
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:counter 0})

    om/IWillMount
    (will-mount [_]
      (let [[ws-in ws-out] (:ws-chs app)
            out (:tweets-ch app)]
        (go
          (>! ws-in {:topic :init :data ""})
          (loop []
            (let [{:keys [topic data] :as package} (<! out)]
              (case topic
                :init (do (om/transact! app :tweets (fn [tweets] (:recent-tweets data)))
                          (om/transact! app :tweet-count (fn [tweets] (:tweet-count data))))
                :new-tweet
                (do (om/transact! app :tweets (fn [tweets] (vec (take 100 (into [data] tweets)))))
                      (om/transact! app :tweet-count inc))))
            (recur)))))
    om/IRenderState
    (render-state [this {:keys [counter] :as state}]
      (om/build tweet-list app {:init-state state}))))


;; --- WS Connection and View Creation ---

(go
  ;; --- create ws connection and dispatch incoming packages to view-channels ---
  (let [connection (<! (connect! (str (if ssl?  "wss://" "ws://")
                                      (.getDomain uri)
                                      ":" 8082 #_(.getPort uri)
                                      "/tweets/ws")))
        in (:in connection)
        out (:out connection)
        stats-ch (:stats-ch @app-state)
        tweets-ch (:tweets-ch @app-state)]
    (do
      (swap! app-state (fn [old params] (assoc old :ws-chs params)) (vec [in out]))
      (go
        (loop []
          (let [{:keys [topic data] :as package} (<! out)]
            (case topic
              :init (>! tweets-ch package)
              :new-tweet (>! tweets-ch package)
              :news-frequencies (>! stats-ch package)
              :news-diffusion (>! stats-ch package)))
          (recur)))))

    ;; --- render views ---
  (om/root
   #(om/component (navbar %))
   app-state
   {:target (. js/document (getElementById "main-navbar"))})

  (om/root
   tweets-view
   app-state
   {:target (. js/document (getElementById "side-container"))})

  )
