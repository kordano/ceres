(ns ceres.core
  (:gen-class :main true)
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [run-server]]
            [ceres.curator :refer [store]]
            [gezwitscher.core :refer [start-filter-stream]]
            [ceres.view :as view]))

(def twitter-state
  {:credentials {:consumer-key (or (System/getenv "TWITTER_API_KEY") "****")
                 :consumer-secret (or (System/getenv "TWITTER_API_SECRET") "****")
                 :access-token (or (System/getenv "TWITTER_ACCESS_TOKEN") "****")
                 :access-token-secret (or (System/getenv "TWITTER_ACCESS_TOKEN_SECRET") "****")}
   :handler (fn [status]
              (do (store status)
                  (println (str "[" (:created_at status) "] Storing " (:id status) " from " (:screen_name (:user status))))))
   :follow [114508061 18016521 5734902 40227292 2834511]
   :track ["@FAZ_NET" "@tagesschau" "@dpa" "@SZ" "@SPIEGELONLINE"]})


(defroutes all-routes
  (resources "/")
  (POST "/detail" [] view/detail)
  (GET "/*" [] (view/page)))


(defn -main [& args]
  (start-filter-stream twitter-state))

(comment

  (def stop-stream (start-filter-stream twitter-state))

  (stop-stream)

  (def server (run-server (site #'all-routes) {:port 8081 :join? false}))

  (server))
