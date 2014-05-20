(ns ceres.core
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [net.cgrand.enlive-html :as enlive]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [clojure.java.io :as io]
            [ceres.warehouse :as warehouse]))

(def news-accounts ["FAZ_NET" "dpa" "tagesschau" "SPIEGELONLINE" "SZ"])

(enlive/deftemplate page
  (io/resource "public/index.html")
  []
  [:body]
  (enlive/append
   (enlive/html
    [:div.panel.panel-default
     [:div.panel-heading
      [:h3.panel-title "Ceres"]]
     [:div.panel-body
      [:div.table-responsive
       [:table.table.table-striped
        [:thead
         [:tr
          (map #(vec [:th %]) news-accounts)]]
        [:tbody
         [:tr
          (map #(vec [:td %]) (warehouse/get-news-counts news-accounts))]]]]
      [:form {:action "detail" :method "POST"}
       [:select.form-control {:name "news-account"}
        (map #(vec [:option {:value %} %]) news-accounts)]
       [:input.btn.btn-primary {:type "submit"}]
       ]]])))


(enlive/deftemplate detail
  (io/resource "public/index.html")
  [{:keys [params]}]
  [:body]
  (enlive/append
   (enlive/html
    [:div.row
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading
       [:h3.panel-title (str "Tweets of @" (:news-account params))]]
      [:div.panel-body
       [:div.table-responsive
        [:table.table.table-striped
         [:tbody
          (map
           #(vec [:form {:method "POST" :action "/detail"}
                  [:tr
                   [:td (:text %)]
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
       [:div.table-responsive
        [:table.table.table-striped
         [:tbody
          (if (:tweet-id params)
            (map
             #(vec [:tr [:td %]])
             (warehouse/find-related-tweets (:tweet-id params)))
            "NuThin")]]]
       ]]])))


(defroutes all-routes
  (resources "/")
  (POST "/detail" [] detail)
  (GET "/*" [] (page)))


#_(def server
    (run-server (site #'all-routes) {:port 8081 :join? false}))

#_(server)
