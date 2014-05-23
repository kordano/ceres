(ns ceres.core
  (:require [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [site]]
            [compojure.core :refer [GET POST defroutes]]
            [org.httpkit.server :refer [with-channel on-close on-receive run-server send!]]
            [ceres.view :as view]))


(defroutes all-routes
  (resources "/")
  (POST "/detail" [] view/detail)
  (GET "/*" [] (view/page)))


#_(def server
    (run-server (site #'all-routes) {:port 8081 :join? false}))

#_(server)
