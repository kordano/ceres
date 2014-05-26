(defproject ceres "0.1.0-SNAPSHOT"

  :description "FIXME: write description"

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.7.0"]
                 [compojure "1.1.8"]
                 [ring "1.2.2"]
                 [enlive "1.1.5"]
                 [http-kit "2.1.18"]
                 [com.novemberain/monger "2.0.0-rc1"]
                 [org.clojure/data.json "0.2.4"]
                 [com.ashafa/clutch "0.4.0-RC1"]
                 [org.twitter4j/twitter4j-core "4.0.1"]
                 [org.twitter4j/twitter4j-stream "4.0.1"]]

  :main ceres.core

  :uberjar-name "ceres-standalone.jar"

  :plugins [[lein-ancient "0.5.4"]])
