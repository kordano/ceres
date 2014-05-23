(ns ceres.view
  (:require [net.cgrand.enlive-html :refer [deftemplate append html]]
            [ceres.warehouse :as warehouse]
            [clojure.java.io :as io]))

(def news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"])

(deftemplate page
  (io/resource "public/index.html")
  []
  [:body]
  (append
   (html
    [:div.container
     [:div.panel.panel-default
      [:div.panel-heading
       [:h3.panel-title "Overall metrics"]]
      [:div.panel-body]
      [:div.table-responsive
       [:table.table.table-striped
        [:thead
         [:tr
          [:th]
          (map #(vec [:th %]) news-accounts)]]
        [:tbody
         [:tr
          [:td [:em "Tweets"]]
          (map #(vec [:td (val %)]) (map count (warehouse/get-news news-accounts)))]
         [:tr
          [:td [:em "Mentions"]]
          (map #(vec [:td %]) (map count (warehouse/get-mentions news-accounts)))]
         [:tr
          [:td [:em "Retweets"]]
          (map #(vec [:td (count (warehouse/get-all-retweets %))]) news-accounts)]
         ]]]]
     [:form {:action "detail" :method "POST"}
      [:select.form-control {:name "news-account"}
       (map #(vec [:option {:value %} %]) news-accounts)]
      [:input.btn.btn-primary {:type "submit"}]]])))

(deftemplate detail
  (io/resource "public/index.html")
  [{:keys [params]}]
  [:body]
  (append
   (html
    [:div.row
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading
       [:a {:href "/"} "Home"]
       [:h3.panel-title (str "Tweets of @" (:news-account params))]]
      [:div.panel-body
       [:div.table-responsive
        [:table.table.table-striped
         [:tbody
          (map
           #(vec [:form {:method "POST" :action "/detail"}
                  [:tr
                   [:td (:text %) [:br] [:small (:created_at %)]]
                   [:input {:type "hidden"
                            :value (:news-account params)
                            :name "news-account"}]
                   [:td
                    [:input.btn.btn-default.btn-xs
                     {:type "submit"
                      :value (:id %)
                      :name "tweet-id"}]]]])
           (warehouse/get-news (:news-account params)))]]]]]
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading
       [:h3.panel-title "Related tweets of " (or (:tweet-id params) "")]]
      [:div.panel-body
       [:dl
        [:dt "Retweeted by"]
        [:dd
         (if (:tweet-id params)
           [:ul.list-group
            (map
             #(vec [:li.list-group-item (str "@" (:screen_name (:user %)) " : " (:text %))])
             (warehouse/find-related-tweets (:tweet-id params)))]
           "")]]]]
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading
       [:h3.panel-title "Mentions @" (:news-account params)]]
      [:div.panel-body
       [:div.table-responsive
        [:table.table.table-striped
         [:tbody
          (map #(vec [:tr
                      [:td (:text %)]])
               (warehouse/get-mentions (:news-account params)))]]]]]])))
