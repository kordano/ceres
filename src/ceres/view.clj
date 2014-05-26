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
      [:div.panel-body]]])))

(deftemplate detail
  (io/resource "public/index.html")
  [{:keys [params]}]
  [:body]
  (append
   (html
    [:div.row
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading]
      [:div.panel-body]]
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading]
      [:div.panel-body]]
     [:div.panel.panel-default.col-md-6
      [:div.panel-heading]
      [:div.panel-body]]])))
