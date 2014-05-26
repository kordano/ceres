(ns ceres.core
  (:gen-class :main true)
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [run-server]]
            [ceres.collector :as collector]
            [ceres.curator :as curator]
            [ceres.view :as view]))


(defroutes all-routes
  (resources "/")
  (POST "/detail" [] view/detail)
  (GET "/*" [] (view/page)))


(defn -main [& args]
  (println "Start streaming...")
  (curator/init-db)
  (collector/do-filter-stream
   [114508061 18016521 5734902 40227292 2834511]
   ["@FAZ_NET" "@tagesschau" "@dpa" "@SZ" "@SPIEGELONLINE"]))

#_(def server

    (run-server (site #'all-routes) {:port 8081 :join? false}))

#_(server)
