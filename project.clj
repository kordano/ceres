(defproject ceres "0.1.0-SNAPSHOT"

  :description "FIXME: write description"

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2227"]
                 [org.clojure/data.json "0.2.4"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [clj-time "0.7.0"]
                 [cheshire "5.3.1"]

                 [compojure "1.1.8"]
                 [ring "1.2.2"]
                 [enlive "1.1.5"]
                 [http-kit "2.1.18"]

                 [net.drib/strokes "0.5.1"]
                 [gezwitscher "0.1.1-SNAPSHOT"]
                 [com.novemberain/monger "2.0.0-rc1"]
                 [incanter "1.5.5"]
                 [kioo "0.4.0"]
                 [figwheel "0.1.3-SNAPSHOT"]
                 [com.facebook/react "0.9.0.2"]
                 [om "0.6.4"]
                 [net.drib/strokes "0.5.1"]

                 [com.cemerick/piggieback "0.1.3"]
                 [weasel "0.2.0"]
                 [com.taoensso/timbre "3.2.1"]]

  :main ceres.core

  :source-paths ["src/cljs" "src/clj"]

  :min-lein-version "2.0.0"

  :uberjar-name "ceres-standalone.jar"

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :plugins [[lein-cljsbuild "1.0.3"]
            [lein-figwheel "0.1.3-SNAPSHOT"]]

  :figwheel {:http-server-root "public"
             :port 3449
             :css-dirs ["resources/public/css"]}

  :cljsbuild
  {:builds
   {:dev
    {:source-paths ["src/cljs"]
     :compiler {:output-to "resources/public/js/compiled/main.js"
                :output-dir "resources/public/js/compiled/out"
                :optimizations :none
                :source-map true}}
    :prod
    {:source-paths ["src/cljs"]
     :compiler {:output-to "resources/public/js/main.js"
                :optimizations :simple}}}})
