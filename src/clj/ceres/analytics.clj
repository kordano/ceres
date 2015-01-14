(ns ceres.analytics
  (:refer-clojure :exclude [find sort])
  (:require [ceres.collector :refer [db]]
            [monger.collection :as mc]
            [clj-time.core :as t]
            [clj-time.periodic :as p]
            [monger.operators :refer :all]
            [monger.query :refer :all]))


(def degrees
  (future
    (->> (mc/find-maps @db "publications")
         (pmap
          (fn [p]
            [(:_id p)
             (+ (mc/count @db "reactions" {:publication (:_id p)})
                (mc/count @db "reactions" {:source (:_id p)}))])))))
