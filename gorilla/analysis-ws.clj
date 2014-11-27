;; gorilla-repl.fileformat = 1

;; **
;;; # News Diffusion on Twitter
;; **

;; @@
(ns unsightly-reserve
  (:refer-clojure :exclude [find sort])
  (:require [gorilla-plot.core :refer :all]
            [ceres.collector :refer :all]
            [ceres.curator :refer [random-tree build-tree-from-source compute-summary]]
            [ceres.analyzer :refer [reaction-tree summary hashtags-of-the-day]]
            [monger.collection :as mc]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clj-time.core :as t]
            [clj-time.periodic :as p]
            [incanter.stats :refer [mean median quantile variance]]
            [monger.operators :refer :all]
            [monger.query :refer :all])
  (:import org.bson.types.ObjectId))
;; @@

;; **
;;; Follwing news distributors are watched
;; **

;; @@
news-accounts
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;dpa&quot;</span>","value":"\"dpa\""},{"type":"html","content":"<span class='clj-string'>&quot;tazgezwitscher&quot;</span>","value":"\"tazgezwitscher\""},{"type":"html","content":"<span class='clj-string'>&quot;BILD&quot;</span>","value":"\"BILD\""},{"type":"html","content":"<span class='clj-string'>&quot;ZDFheute&quot;</span>","value":"\"ZDFheute\""},{"type":"html","content":"<span class='clj-string'>&quot;FAZ_NET&quot;</span>","value":"\"FAZ_NET\""},{"type":"html","content":"<span class='clj-string'>&quot;focusonline&quot;</span>","value":"\"focusonline\""},{"type":"html","content":"<span class='clj-string'>&quot;welt&quot;</span>","value":"\"welt\""},{"type":"html","content":"<span class='clj-string'>&quot;N24_de&quot;</span>","value":"\"N24_de\""},{"type":"html","content":"<span class='clj-string'>&quot;DerWesten&quot;</span>","value":"\"DerWesten\""},{"type":"html","content":"<span class='clj-string'>&quot;SPIEGELONLINE&quot;</span>","value":"\"SPIEGELONLINE\""},{"type":"html","content":"<span class='clj-string'>&quot;ntvde&quot;</span>","value":"\"ntvde\""},{"type":"html","content":"<span class='clj-string'>&quot;tagesschau&quot;</span>","value":"\"tagesschau\""},{"type":"html","content":"<span class='clj-string'>&quot;SZ&quot;</span>","value":"\"SZ\""},{"type":"html","content":"<span class='clj-string'>&quot;sternde&quot;</span>","value":"\"sternde\""}],"value":"#{\"dpa\" \"tazgezwitscher\" \"BILD\" \"ZDFheute\" \"FAZ_NET\" \"focusonline\" \"welt\" \"N24_de\" \"DerWesten\" \"SPIEGELONLINE\" \"ntvde\" \"tagesschau\" \"SZ\" \"sternde\"}"}
;; <=

;; **
;;; ## Average Tweets per Day
;; **

;; @@
(def start-date (t/date-time 2014 8 1))
(def end-date (t/date-time 2014 9 1))
(def days-running (t/in-days (t/interval start-date end-date)))
(def dates (take days-running (p/periodic-seq start-date (t/days 1))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/dates</span>","value":"#'unsightly-reserve/dates"}
;; <=

;; **
;;; ### Combined
;; **

;; @@
(let [tweet-count (map #(mc/count @db "tweets" {:created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        days (range  2 (inc (inc days-running)))]
    (list-plot tweet-count :joined true :plot-size 900))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3","values":[{"x":0,"y":8774},{"x":1,"y":6783},{"x":2,"y":9144},{"x":3,"y":9718},{"x":4,"y":9909},{"x":5,"y":11237},{"x":6,"y":10819},{"x":7,"y":9143},{"x":8,"y":6782},{"x":9,"y":8274},{"x":10,"y":7935},{"x":11,"y":12168},{"x":12,"y":11173},{"x":13,"y":10445},{"x":14,"y":10248},{"x":15,"y":7808},{"x":16,"y":7729},{"x":17,"y":8024},{"x":18,"y":12345},{"x":19,"y":9617},{"x":20,"y":8439},{"x":21,"y":8807},{"x":22,"y":7077},{"x":23,"y":7140},{"x":24,"y":8550},{"x":25,"y":9374},{"x":26,"y":9221},{"x":27,"y":11006},{"x":28,"y":9324},{"x":29,"y":7472},{"x":30,"y":10782}]}],"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"23ee71dd-a4aa-4eb4-85f0-e58db8277dc3\", :values ({:x 0, :y 8774} {:x 1, :y 6783} {:x 2, :y 9144} {:x 3, :y 9718} {:x 4, :y 9909} {:x 5, :y 11237} {:x 6, :y 10819} {:x 7, :y 9143} {:x 8, :y 6782} {:x 9, :y 8274} {:x 10, :y 7935} {:x 11, :y 12168} {:x 12, :y 11173} {:x 13, :y 10445} {:x 14, :y 10248} {:x 15, :y 7808} {:x 16, :y 7729} {:x 17, :y 8024} {:x 18, :y 12345} {:x 19, :y 9617} {:x 20, :y 8439} {:x 21, :y 8807} {:x 22, :y 7077} {:x 23, :y 7140} {:x 24, :y 8550} {:x 25, :y 9374} {:x 26, :y 9221} {:x 27, :y 11006} {:x 28, :y 9324} {:x 29, :y 7472} {:x 30, :y 10782})}], :width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ### Daily Newspapers 1
;;; Daily tweet counts of SZ, FAZ and Bild.
;; **

;; @@
(let [sz-counts (map #(mc/count @db "tweets" {:user.screen_name "SZ" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      faz-counts (map #(mc/count @db "tweets" {:user.screen_name "FAZ_NET" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
       bild-counts (map #(mc/count @db "tweets" {:user.screen_name "BILD" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)]
    (compose
      (list-plot sz-counts :joined true :color "red" :plot-size 900 :plot-range [[0 30] [0 140]])
      (list-plot faz-counts :joined true :color "green")
      (list-plot bild-counts :joined true :color "blue")))
;; @@
;; =>
;;; {"type":"vega","content":{"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,30]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,140]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"1f7509a5-d518-4c30-bf36-904faf4a82c9","values":[{"x":0,"y":62},{"x":1,"y":40},{"x":2,"y":35},{"x":3,"y":53},{"x":4,"y":66},{"x":5,"y":76},{"x":6,"y":65},{"x":7,"y":78},{"x":8,"y":42},{"x":9,"y":40},{"x":10,"y":71},{"x":11,"y":63},{"x":12,"y":46},{"x":13,"y":55},{"x":14,"y":65},{"x":15,"y":36},{"x":16,"y":46},{"x":17,"y":34},{"x":18,"y":35},{"x":19,"y":58},{"x":20,"y":71},{"x":21,"y":58},{"x":22,"y":2},{"x":23,"y":0},{"x":24,"y":78},{"x":25,"y":73},{"x":26,"y":73},{"x":27,"y":72},{"x":28,"y":41},{"x":29,"y":46},{"x":30,"y":35}]},{"name":"3fb82382-7287-4da2-bfca-ff5d5a12efbe","values":[{"x":0,"y":118},{"x":1,"y":61},{"x":2,"y":66},{"x":3,"y":95},{"x":4,"y":96},{"x":5,"y":97},{"x":6,"y":102},{"x":7,"y":93},{"x":8,"y":68},{"x":9,"y":67},{"x":10,"y":92},{"x":11,"y":110},{"x":12,"y":108},{"x":13,"y":110},{"x":14,"y":100},{"x":15,"y":69},{"x":16,"y":73},{"x":17,"y":89},{"x":18,"y":105},{"x":19,"y":104},{"x":20,"y":112},{"x":21,"y":108},{"x":22,"y":74},{"x":23,"y":70},{"x":24,"y":101},{"x":25,"y":113},{"x":26,"y":117},{"x":27,"y":122},{"x":28,"y":117},{"x":29,"y":78},{"x":30,"y":84}]},{"name":"4268b469-3e96-47fc-bcc5-29aef47fe837","values":[{"x":0,"y":50},{"x":1,"y":21},{"x":2,"y":30},{"x":3,"y":41},{"x":4,"y":49},{"x":5,"y":48},{"x":6,"y":45},{"x":7,"y":44},{"x":8,"y":38},{"x":9,"y":35},{"x":10,"y":42},{"x":11,"y":50},{"x":12,"y":55},{"x":13,"y":50},{"x":14,"y":35},{"x":15,"y":38},{"x":16,"y":23},{"x":17,"y":37},{"x":18,"y":50},{"x":19,"y":44},{"x":20,"y":48},{"x":21,"y":51},{"x":22,"y":44},{"x":23,"y":58},{"x":24,"y":45},{"x":25,"y":53},{"x":26,"y":49},{"x":27,"y":47},{"x":28,"y":37},{"x":29,"y":43},{"x":30,"y":27}]}],"marks":[{"type":"line","from":{"data":"1f7509a5-d518-4c30-bf36-904faf4a82c9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"3fb82382-7287-4da2-bfca-ff5d5a12efbe"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"4268b469-3e96-47fc-bcc5-29aef47fe837"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 30]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 140]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"1f7509a5-d518-4c30-bf36-904faf4a82c9\", :values ({:x 0, :y 62} {:x 1, :y 40} {:x 2, :y 35} {:x 3, :y 53} {:x 4, :y 66} {:x 5, :y 76} {:x 6, :y 65} {:x 7, :y 78} {:x 8, :y 42} {:x 9, :y 40} {:x 10, :y 71} {:x 11, :y 63} {:x 12, :y 46} {:x 13, :y 55} {:x 14, :y 65} {:x 15, :y 36} {:x 16, :y 46} {:x 17, :y 34} {:x 18, :y 35} {:x 19, :y 58} {:x 20, :y 71} {:x 21, :y 58} {:x 22, :y 2} {:x 23, :y 0} {:x 24, :y 78} {:x 25, :y 73} {:x 26, :y 73} {:x 27, :y 72} {:x 28, :y 41} {:x 29, :y 46} {:x 30, :y 35})} {:name \"3fb82382-7287-4da2-bfca-ff5d5a12efbe\", :values ({:x 0, :y 118} {:x 1, :y 61} {:x 2, :y 66} {:x 3, :y 95} {:x 4, :y 96} {:x 5, :y 97} {:x 6, :y 102} {:x 7, :y 93} {:x 8, :y 68} {:x 9, :y 67} {:x 10, :y 92} {:x 11, :y 110} {:x 12, :y 108} {:x 13, :y 110} {:x 14, :y 100} {:x 15, :y 69} {:x 16, :y 73} {:x 17, :y 89} {:x 18, :y 105} {:x 19, :y 104} {:x 20, :y 112} {:x 21, :y 108} {:x 22, :y 74} {:x 23, :y 70} {:x 24, :y 101} {:x 25, :y 113} {:x 26, :y 117} {:x 27, :y 122} {:x 28, :y 117} {:x 29, :y 78} {:x 30, :y 84})} {:name \"4268b469-3e96-47fc-bcc5-29aef47fe837\", :values ({:x 0, :y 50} {:x 1, :y 21} {:x 2, :y 30} {:x 3, :y 41} {:x 4, :y 49} {:x 5, :y 48} {:x 6, :y 45} {:x 7, :y 44} {:x 8, :y 38} {:x 9, :y 35} {:x 10, :y 42} {:x 11, :y 50} {:x 12, :y 55} {:x 13, :y 50} {:x 14, :y 35} {:x 15, :y 38} {:x 16, :y 23} {:x 17, :y 37} {:x 18, :y 50} {:x 19, :y 44} {:x 20, :y 48} {:x 21, :y 51} {:x 22, :y 44} {:x 23, :y 58} {:x 24, :y 45} {:x 25, :y 53} {:x 26, :y 49} {:x 27, :y 47} {:x 28, :y 37} {:x 29, :y 43} {:x 30, :y 27})}), :marks ({:type \"line\", :from {:data \"1f7509a5-d518-4c30-bf36-904faf4a82c9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"3fb82382-7287-4da2-bfca-ff5d5a12efbe\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"4268b469-3e96-47fc-bcc5-29aef47fe837\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; ### Daily Newspaper 2
;;; Daily tweet counts of Welt, Westen and taz.
;; **

;; @@
(let [welt-counts (map #(mc/count @db "tweets" {:user.screen_name "welt" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      taz-counts (map #(mc/count @db "tweets" {:user.screen_name "tazgezwitscher" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      westen-counts (map #(mc/count @db "tweets" {:user.screen_name "DerWesten" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)]
  (compose
      (list-plot welt-counts :joined true :color "red" :plot-size 900 :plot-range [[0 30] [0 140]])
      (list-plot taz-counts :joined true :color "green")
      (list-plot westen-counts :joined true :color "blue")))
;; @@
;; =>
;;; {"type":"vega","content":{"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,30]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,140]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"480cf0e2-11b1-42cd-a42e-8b9d23b8be32","values":[{"x":0,"y":52},{"x":1,"y":42},{"x":2,"y":57},{"x":3,"y":60},{"x":4,"y":66},{"x":5,"y":61},{"x":6,"y":77},{"x":7,"y":75},{"x":8,"y":56},{"x":9,"y":60},{"x":10,"y":51},{"x":11,"y":67},{"x":12,"y":73},{"x":13,"y":82},{"x":14,"y":68},{"x":15,"y":32},{"x":16,"y":59},{"x":17,"y":69},{"x":18,"y":92},{"x":19,"y":74},{"x":20,"y":73},{"x":21,"y":51},{"x":22,"y":68},{"x":23,"y":63},{"x":24,"y":69},{"x":25,"y":69},{"x":26,"y":63},{"x":27,"y":79},{"x":28,"y":68},{"x":29,"y":55},{"x":30,"y":59}]},{"name":"9b1a5002-100b-4a23-b57d-371d658575a7","values":[{"x":0,"y":22},{"x":1,"y":15},{"x":2,"y":12},{"x":3,"y":22},{"x":4,"y":22},{"x":5,"y":23},{"x":6,"y":30},{"x":7,"y":19},{"x":8,"y":15},{"x":9,"y":16},{"x":10,"y":17},{"x":11,"y":25},{"x":12,"y":21},{"x":13,"y":25},{"x":14,"y":31},{"x":15,"y":14},{"x":16,"y":15},{"x":17,"y":20},{"x":18,"y":32},{"x":19,"y":27},{"x":20,"y":40},{"x":21,"y":29},{"x":22,"y":16},{"x":23,"y":18},{"x":24,"y":24},{"x":25,"y":31},{"x":26,"y":27},{"x":27,"y":37},{"x":28,"y":25},{"x":29,"y":3},{"x":30,"y":27}]},{"name":"1fc1967c-bfdd-4890-9fcd-b33ba19dd159","values":[{"x":0,"y":41},{"x":1,"y":16},{"x":2,"y":21},{"x":3,"y":31},{"x":4,"y":34},{"x":5,"y":33},{"x":6,"y":31},{"x":7,"y":29},{"x":8,"y":12},{"x":9,"y":17},{"x":10,"y":32},{"x":11,"y":31},{"x":12,"y":32},{"x":13,"y":52},{"x":14,"y":37},{"x":15,"y":13},{"x":16,"y":17},{"x":17,"y":35},{"x":18,"y":31},{"x":19,"y":32},{"x":20,"y":36},{"x":21,"y":49},{"x":22,"y":26},{"x":23,"y":27},{"x":24,"y":31},{"x":25,"y":31},{"x":26,"y":29},{"x":27,"y":48},{"x":28,"y":48},{"x":29,"y":24},{"x":30,"y":21}]}],"marks":[{"type":"line","from":{"data":"480cf0e2-11b1-42cd-a42e-8b9d23b8be32"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"9b1a5002-100b-4a23-b57d-371d658575a7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"1fc1967c-bfdd-4890-9fcd-b33ba19dd159"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 30]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 140]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"480cf0e2-11b1-42cd-a42e-8b9d23b8be32\", :values ({:x 0, :y 52} {:x 1, :y 42} {:x 2, :y 57} {:x 3, :y 60} {:x 4, :y 66} {:x 5, :y 61} {:x 6, :y 77} {:x 7, :y 75} {:x 8, :y 56} {:x 9, :y 60} {:x 10, :y 51} {:x 11, :y 67} {:x 12, :y 73} {:x 13, :y 82} {:x 14, :y 68} {:x 15, :y 32} {:x 16, :y 59} {:x 17, :y 69} {:x 18, :y 92} {:x 19, :y 74} {:x 20, :y 73} {:x 21, :y 51} {:x 22, :y 68} {:x 23, :y 63} {:x 24, :y 69} {:x 25, :y 69} {:x 26, :y 63} {:x 27, :y 79} {:x 28, :y 68} {:x 29, :y 55} {:x 30, :y 59})} {:name \"9b1a5002-100b-4a23-b57d-371d658575a7\", :values ({:x 0, :y 22} {:x 1, :y 15} {:x 2, :y 12} {:x 3, :y 22} {:x 4, :y 22} {:x 5, :y 23} {:x 6, :y 30} {:x 7, :y 19} {:x 8, :y 15} {:x 9, :y 16} {:x 10, :y 17} {:x 11, :y 25} {:x 12, :y 21} {:x 13, :y 25} {:x 14, :y 31} {:x 15, :y 14} {:x 16, :y 15} {:x 17, :y 20} {:x 18, :y 32} {:x 19, :y 27} {:x 20, :y 40} {:x 21, :y 29} {:x 22, :y 16} {:x 23, :y 18} {:x 24, :y 24} {:x 25, :y 31} {:x 26, :y 27} {:x 27, :y 37} {:x 28, :y 25} {:x 29, :y 3} {:x 30, :y 27})} {:name \"1fc1967c-bfdd-4890-9fcd-b33ba19dd159\", :values ({:x 0, :y 41} {:x 1, :y 16} {:x 2, :y 21} {:x 3, :y 31} {:x 4, :y 34} {:x 5, :y 33} {:x 6, :y 31} {:x 7, :y 29} {:x 8, :y 12} {:x 9, :y 17} {:x 10, :y 32} {:x 11, :y 31} {:x 12, :y 32} {:x 13, :y 52} {:x 14, :y 37} {:x 15, :y 13} {:x 16, :y 17} {:x 17, :y 35} {:x 18, :y 31} {:x 19, :y 32} {:x 20, :y 36} {:x 21, :y 49} {:x 22, :y 26} {:x 23, :y 27} {:x 24, :y 31} {:x 25, :y 31} {:x 26, :y 29} {:x 27, :y 48} {:x 28, :y 48} {:x 29, :y 24} {:x 30, :y 21})}), :marks ({:type \"line\", :from {:data \"480cf0e2-11b1-42cd-a42e-8b9d23b8be32\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"9b1a5002-100b-4a23-b57d-371d658575a7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"1fc1967c-bfdd-4890-9fcd-b33ba19dd159\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; ### Weekly Newsmagazines
;;; Daily tweet counts of Spiegel, Stern and Focus.
;; **

;; @@
(let [spon-counts (map #(mc/count @db "tweets" {:user.screen_name "SPIEGELONLINE" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      stern-counts (map #(mc/count @db "tweets" {:user.screen_name "sternde" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      focus-counts (map #(mc/count @db "tweets" {:user.screen_name "focusonline" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)]
  (compose
    (list-plot spon-counts :joined true :color "red" :plot-size 900 :plot-range [[0 30] [0 130]])
      (list-plot stern-counts :joined true :color "green")
      (list-plot focus-counts :joined true :color "blue")))
;; @@
;; =>
;;; {"type":"vega","content":{"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,30]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,130]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"f36052e1-0f00-473d-b6a8-007d1716f371","values":[{"x":0,"y":66},{"x":1,"y":43},{"x":2,"y":53},{"x":3,"y":61},{"x":4,"y":50},{"x":5,"y":69},{"x":6,"y":60},{"x":7,"y":50},{"x":8,"y":44},{"x":9,"y":37},{"x":10,"y":49},{"x":11,"y":62},{"x":12,"y":68},{"x":13,"y":58},{"x":14,"y":61},{"x":15,"y":53},{"x":16,"y":42},{"x":17,"y":54},{"x":18,"y":59},{"x":19,"y":42},{"x":20,"y":56},{"x":21,"y":55},{"x":22,"y":55},{"x":23,"y":56},{"x":24,"y":50},{"x":25,"y":62},{"x":26,"y":54},{"x":27,"y":60},{"x":28,"y":82},{"x":29,"y":70},{"x":30,"y":82}]},{"name":"c4876bf1-d848-4234-af86-22f068af2ef9","values":[{"x":0,"y":24},{"x":1,"y":17},{"x":2,"y":30},{"x":3,"y":46},{"x":4,"y":39},{"x":5,"y":43},{"x":6,"y":46},{"x":7,"y":36},{"x":8,"y":20},{"x":9,"y":11},{"x":10,"y":32},{"x":11,"y":34},{"x":12,"y":36},{"x":13,"y":27},{"x":14,"y":28},{"x":15,"y":26},{"x":16,"y":23},{"x":17,"y":39},{"x":18,"y":42},{"x":19,"y":31},{"x":20,"y":26},{"x":21,"y":25},{"x":22,"y":17},{"x":23,"y":15},{"x":24,"y":40},{"x":25,"y":37},{"x":26,"y":41},{"x":27,"y":40},{"x":28,"y":32},{"x":29,"y":43},{"x":30,"y":36}]},{"name":"6c10c39d-4d0f-4647-9c0e-a7f47e302d98","values":[{"x":0,"y":8},{"x":1,"y":2},{"x":2,"y":6},{"x":3,"y":6},{"x":4,"y":8},{"x":5,"y":9},{"x":6,"y":9},{"x":7,"y":12},{"x":8,"y":4},{"x":9,"y":8},{"x":10,"y":7},{"x":11,"y":13},{"x":12,"y":9},{"x":13,"y":9},{"x":14,"y":9},{"x":15,"y":4},{"x":16,"y":10},{"x":17,"y":10},{"x":18,"y":15},{"x":19,"y":12},{"x":20,"y":8},{"x":21,"y":9},{"x":22,"y":8},{"x":23,"y":7},{"x":24,"y":17},{"x":25,"y":10},{"x":26,"y":9},{"x":27,"y":7},{"x":28,"y":6},{"x":29,"y":12},{"x":30,"y":8}]}],"marks":[{"type":"line","from":{"data":"f36052e1-0f00-473d-b6a8-007d1716f371"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"c4876bf1-d848-4234-af86-22f068af2ef9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"6c10c39d-4d0f-4647-9c0e-a7f47e302d98"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 30]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 130]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"f36052e1-0f00-473d-b6a8-007d1716f371\", :values ({:x 0, :y 66} {:x 1, :y 43} {:x 2, :y 53} {:x 3, :y 61} {:x 4, :y 50} {:x 5, :y 69} {:x 6, :y 60} {:x 7, :y 50} {:x 8, :y 44} {:x 9, :y 37} {:x 10, :y 49} {:x 11, :y 62} {:x 12, :y 68} {:x 13, :y 58} {:x 14, :y 61} {:x 15, :y 53} {:x 16, :y 42} {:x 17, :y 54} {:x 18, :y 59} {:x 19, :y 42} {:x 20, :y 56} {:x 21, :y 55} {:x 22, :y 55} {:x 23, :y 56} {:x 24, :y 50} {:x 25, :y 62} {:x 26, :y 54} {:x 27, :y 60} {:x 28, :y 82} {:x 29, :y 70} {:x 30, :y 82})} {:name \"c4876bf1-d848-4234-af86-22f068af2ef9\", :values ({:x 0, :y 24} {:x 1, :y 17} {:x 2, :y 30} {:x 3, :y 46} {:x 4, :y 39} {:x 5, :y 43} {:x 6, :y 46} {:x 7, :y 36} {:x 8, :y 20} {:x 9, :y 11} {:x 10, :y 32} {:x 11, :y 34} {:x 12, :y 36} {:x 13, :y 27} {:x 14, :y 28} {:x 15, :y 26} {:x 16, :y 23} {:x 17, :y 39} {:x 18, :y 42} {:x 19, :y 31} {:x 20, :y 26} {:x 21, :y 25} {:x 22, :y 17} {:x 23, :y 15} {:x 24, :y 40} {:x 25, :y 37} {:x 26, :y 41} {:x 27, :y 40} {:x 28, :y 32} {:x 29, :y 43} {:x 30, :y 36})} {:name \"6c10c39d-4d0f-4647-9c0e-a7f47e302d98\", :values ({:x 0, :y 8} {:x 1, :y 2} {:x 2, :y 6} {:x 3, :y 6} {:x 4, :y 8} {:x 5, :y 9} {:x 6, :y 9} {:x 7, :y 12} {:x 8, :y 4} {:x 9, :y 8} {:x 10, :y 7} {:x 11, :y 13} {:x 12, :y 9} {:x 13, :y 9} {:x 14, :y 9} {:x 15, :y 4} {:x 16, :y 10} {:x 17, :y 10} {:x 18, :y 15} {:x 19, :y 12} {:x 20, :y 8} {:x 21, :y 9} {:x 22, :y 8} {:x 23, :y 7} {:x 24, :y 17} {:x 25, :y 10} {:x 26, :y 9} {:x 27, :y 7} {:x 28, :y 6} {:x 29, :y 12} {:x 30, :y 8})}), :marks ({:type \"line\", :from {:data \"f36052e1-0f00-473d-b6a8-007d1716f371\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"c4876bf1-d848-4234-af86-22f068af2ef9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"6c10c39d-4d0f-4647-9c0e-a7f47e302d98\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; ### TV News
;; **

;; @@
(let [tagesschau-counts (map #(mc/count @db "tweets" {:user.screen_name "tagesschau" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      zdf-counts (map #(mc/count @db "tweets" {:user.screen_name "ZDFheute" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      n24-counts (map #(mc/count @db "tweets" {:user.screen_name "N24_de" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
      ntv-counts (map #(mc/count @db "tweets" {:user.screen_name "ntvde" :created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)]
  (compose
    (list-plot tagesschau-counts :joined true :color "red" :plot-size 1000 :plot-range [[0 30] [0 210]])
    (list-plot zdf-counts :joined true :color "green")
    (list-plot n24-counts :joined true :color "orange")
    (list-plot ntv-counts :joined true :color "blue")))
;; @@
;; =>
;;; {"type":"vega","content":{"width":1000,"height":618.0469970703125,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,30]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,210]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"da5b4b29-4b01-4e60-bd31-af293dfd84ee","values":[{"x":0,"y":84},{"x":1,"y":32},{"x":2,"y":42},{"x":3,"y":52},{"x":4,"y":73},{"x":5,"y":66},{"x":6,"y":47},{"x":7,"y":66},{"x":8,"y":41},{"x":9,"y":42},{"x":10,"y":45},{"x":11,"y":56},{"x":12,"y":47},{"x":13,"y":61},{"x":14,"y":74},{"x":15,"y":39},{"x":16,"y":21},{"x":17,"y":59},{"x":18,"y":71},{"x":19,"y":65},{"x":20,"y":68},{"x":21,"y":53},{"x":22,"y":36},{"x":23,"y":28},{"x":24,"y":54},{"x":25,"y":66},{"x":26,"y":50},{"x":27,"y":91},{"x":28,"y":52},{"x":29,"y":32},{"x":30,"y":123}]},{"name":"d58a13c0-0b7a-4488-a2f0-8700e52c0802","values":[{"x":0,"y":33},{"x":1,"y":27},{"x":2,"y":24},{"x":3,"y":42},{"x":4,"y":32},{"x":5,"y":26},{"x":6,"y":28},{"x":7,"y":37},{"x":8,"y":21},{"x":9,"y":18},{"x":10,"y":30},{"x":11,"y":31},{"x":12,"y":45},{"x":13,"y":40},{"x":14,"y":28},{"x":15,"y":30},{"x":16,"y":31},{"x":17,"y":25},{"x":18,"y":45},{"x":19,"y":37},{"x":20,"y":29},{"x":21,"y":26},{"x":22,"y":17},{"x":23,"y":17},{"x":24,"y":26},{"x":25,"y":50},{"x":26,"y":27},{"x":27,"y":24},{"x":28,"y":33},{"x":29,"y":14},{"x":30,"y":56}]},{"name":"d0ee2b2c-e64b-47e0-8d90-40ea71dc5d1e","values":[{"x":0,"y":87},{"x":1,"y":50},{"x":2,"y":56},{"x":3,"y":75},{"x":4,"y":67},{"x":5,"y":83},{"x":6,"y":72},{"x":7,"y":58},{"x":8,"y":43},{"x":9,"y":53},{"x":10,"y":58},{"x":11,"y":65},{"x":12,"y":65},{"x":13,"y":61},{"x":14,"y":75},{"x":15,"y":53},{"x":16,"y":49},{"x":17,"y":60},{"x":18,"y":76},{"x":19,"y":74},{"x":20,"y":63},{"x":21,"y":77},{"x":22,"y":47},{"x":23,"y":46},{"x":24,"y":68},{"x":25,"y":73},{"x":26,"y":69},{"x":27,"y":73},{"x":28,"y":61},{"x":29,"y":48},{"x":30,"y":57}]},{"name":"bc53801a-623f-4018-8cc1-7ed595eaf593","values":[{"x":0,"y":49},{"x":1,"y":43},{"x":2,"y":43},{"x":3,"y":43},{"x":4,"y":50},{"x":5,"y":42},{"x":6,"y":44},{"x":7,"y":46},{"x":8,"y":34},{"x":9,"y":41},{"x":10,"y":47},{"x":11,"y":48},{"x":12,"y":68},{"x":13,"y":113},{"x":14,"y":114},{"x":15,"y":49},{"x":16,"y":28},{"x":17,"y":50},{"x":18,"y":62},{"x":19,"y":63},{"x":20,"y":61},{"x":21,"y":75},{"x":22,"y":70},{"x":23,"y":57},{"x":24,"y":72},{"x":25,"y":73},{"x":26,"y":66},{"x":27,"y":77},{"x":28,"y":67},{"x":29,"y":62},{"x":30,"y":59}]}],"marks":[{"type":"line","from":{"data":"da5b4b29-4b01-4e60-bd31-af293dfd84ee"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"d58a13c0-0b7a-4488-a2f0-8700e52c0802"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"d0ee2b2c-e64b-47e0-8d90-40ea71dc5d1e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"orange"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"bc53801a-623f-4018-8cc1-7ed595eaf593"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 1000, :height 618.047, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 30]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 210]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"da5b4b29-4b01-4e60-bd31-af293dfd84ee\", :values ({:x 0, :y 84} {:x 1, :y 32} {:x 2, :y 42} {:x 3, :y 52} {:x 4, :y 73} {:x 5, :y 66} {:x 6, :y 47} {:x 7, :y 66} {:x 8, :y 41} {:x 9, :y 42} {:x 10, :y 45} {:x 11, :y 56} {:x 12, :y 47} {:x 13, :y 61} {:x 14, :y 74} {:x 15, :y 39} {:x 16, :y 21} {:x 17, :y 59} {:x 18, :y 71} {:x 19, :y 65} {:x 20, :y 68} {:x 21, :y 53} {:x 22, :y 36} {:x 23, :y 28} {:x 24, :y 54} {:x 25, :y 66} {:x 26, :y 50} {:x 27, :y 91} {:x 28, :y 52} {:x 29, :y 32} {:x 30, :y 123})} {:name \"d58a13c0-0b7a-4488-a2f0-8700e52c0802\", :values ({:x 0, :y 33} {:x 1, :y 27} {:x 2, :y 24} {:x 3, :y 42} {:x 4, :y 32} {:x 5, :y 26} {:x 6, :y 28} {:x 7, :y 37} {:x 8, :y 21} {:x 9, :y 18} {:x 10, :y 30} {:x 11, :y 31} {:x 12, :y 45} {:x 13, :y 40} {:x 14, :y 28} {:x 15, :y 30} {:x 16, :y 31} {:x 17, :y 25} {:x 18, :y 45} {:x 19, :y 37} {:x 20, :y 29} {:x 21, :y 26} {:x 22, :y 17} {:x 23, :y 17} {:x 24, :y 26} {:x 25, :y 50} {:x 26, :y 27} {:x 27, :y 24} {:x 28, :y 33} {:x 29, :y 14} {:x 30, :y 56})} {:name \"d0ee2b2c-e64b-47e0-8d90-40ea71dc5d1e\", :values ({:x 0, :y 87} {:x 1, :y 50} {:x 2, :y 56} {:x 3, :y 75} {:x 4, :y 67} {:x 5, :y 83} {:x 6, :y 72} {:x 7, :y 58} {:x 8, :y 43} {:x 9, :y 53} {:x 10, :y 58} {:x 11, :y 65} {:x 12, :y 65} {:x 13, :y 61} {:x 14, :y 75} {:x 15, :y 53} {:x 16, :y 49} {:x 17, :y 60} {:x 18, :y 76} {:x 19, :y 74} {:x 20, :y 63} {:x 21, :y 77} {:x 22, :y 47} {:x 23, :y 46} {:x 24, :y 68} {:x 25, :y 73} {:x 26, :y 69} {:x 27, :y 73} {:x 28, :y 61} {:x 29, :y 48} {:x 30, :y 57})} {:name \"bc53801a-623f-4018-8cc1-7ed595eaf593\", :values ({:x 0, :y 49} {:x 1, :y 43} {:x 2, :y 43} {:x 3, :y 43} {:x 4, :y 50} {:x 5, :y 42} {:x 6, :y 44} {:x 7, :y 46} {:x 8, :y 34} {:x 9, :y 41} {:x 10, :y 47} {:x 11, :y 48} {:x 12, :y 68} {:x 13, :y 113} {:x 14, :y 114} {:x 15, :y 49} {:x 16, :y 28} {:x 17, :y 50} {:x 18, :y 62} {:x 19, :y 63} {:x 20, :y 61} {:x 21, :y 75} {:x 22, :y 70} {:x 23, :y 57} {:x 24, :y 72} {:x 25, :y 73} {:x 26, :y 66} {:x 27, :y 77} {:x 28, :y 67} {:x 29, :y 62} {:x 30, :y 59})}), :marks ({:type \"line\", :from {:data \"da5b4b29-4b01-4e60-bd31-af293dfd84ee\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"d58a13c0-0b7a-4488-a2f0-8700e52c0802\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"d0ee2b2c-e64b-47e0-8d90-40ea71dc5d1e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"orange\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"bc53801a-623f-4018-8cc1-7ed595eaf593\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; ## Average Tweets per Hour
;; **

;; @@
(let [days-running (t/in-hours (t/interval start-date end-date))
        dates (take days-running (p/periodic-seq start-date (t/hours 1)))
        tweet-count (map #(mc/count @db "tweets" {:created_at {$gte % $lt (t/plus % (t/hours 1))}}) dates)
        times (range days-running)
        tweet-count-2 (loop [hours-list []
                             tweet-dist tweet-count]
                        (if (empty? tweet-dist)
                          hours-list
                          (recur (conj hours-list (vec (take 24 tweet-dist))) (drop 24 tweet-dist))))
        avg-tweets-per-hour (map (fn [hour] (/ (reduce + (map (fn [count] (get count hour)) tweet-count-2)) (count tweet-count-2))) (range 24))]
  (list-plot avg-tweets-per-hour :joined true :plot-size 900))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4ea2bc93-721e-44fb-bd1a-51f2088042c2","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"4ea2bc93-721e-44fb-bd1a-51f2088042c2","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"4ea2bc93-721e-44fb-bd1a-51f2088042c2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"4ea2bc93-721e-44fb-bd1a-51f2088042c2","values":[{"x":0,"y":77.16129032258065},{"x":1,"y":51.80645161290323},{"x":2,"y":49.19354838709677},{"x":3,"y":59.48387096774194},{"x":4,"y":108.258064516129},{"x":5,"y":236},{"x":6,"y":431.1290322580645},{"x":7,"y":554.0322580645161},{"x":8,"y":572.1290322580645},{"x":9,"y":597.9677419354839},{"x":10,"y":605.3870967741935},{"x":11,"y":592.0322580645161},{"x":12,"y":605.2258064516129},{"x":13,"y":565.4193548387098},{"x":14,"y":519.8064516129032},{"x":15,"y":546.1935483870968},{"x":16,"y":549.8064516129032},{"x":17,"y":513.2258064516129},{"x":18,"y":461.8387096774194},{"x":19,"y":444.741935483871},{"x":20,"y":425.8387096774194},{"x":21,"y":313.4193548387097},{"x":22,"y":202.2258064516129},{"x":23,"y":119.8387096774194}]}],"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4ea2bc93-721e-44fb-bd1a-51f2088042c2\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"4ea2bc93-721e-44fb-bd1a-51f2088042c2\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"4ea2bc93-721e-44fb-bd1a-51f2088042c2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4ea2bc93-721e-44fb-bd1a-51f2088042c2\", :values ({:x 0, :y 2392/31} {:x 1, :y 1606/31} {:x 2, :y 1525/31} {:x 3, :y 1844/31} {:x 4, :y 3356/31} {:x 5, :y 236} {:x 6, :y 13365/31} {:x 7, :y 17175/31} {:x 8, :y 17736/31} {:x 9, :y 18537/31} {:x 10, :y 18767/31} {:x 11, :y 18353/31} {:x 12, :y 18762/31} {:x 13, :y 17528/31} {:x 14, :y 16114/31} {:x 15, :y 16932/31} {:x 16, :y 17044/31} {:x 17, :y 15910/31} {:x 18, :y 14317/31} {:x 19, :y 13787/31} {:x 20, :y 13201/31} {:x 21, :y 9716/31} {:x 22, :y 6269/31} {:x 23, :y 3715/31})}], :width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(let [news-tweets (mc/count @db "tweets" {:user.screen_name {$in news-accounts}
                                          :created_at {$gt start-date
                                                       $lt end-date}})
      overall (mc/count @db "tweets" {:created_at {$gt start-date
                                                       $lt end-date}})]
  (float (/ news-tweets overall)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0.06834649</span>","value":"0.06834649"}
;; <=

;; **
;;; ## Graph Analysis
;;; In the following section the individual graphs and subgraphs are examined. First basic metrics such as degree, depth and average connectivity are computed.
;;; 
;;; An example of an individual tree is as follows:
;; **

;; @@
(def some-tree (build-tree-from-source (ObjectId. "53f33d9ee4b0c2a12eb4a339")))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/some-tree</span>","value":"#'unsightly-reserve/some-tree"}
;; <=

;; **
;;; Basic Analysis has following features
;; **

;; @@
(:title some-tree)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;Ferguson : Der Tag, an dem die US-Polizei mein Feind wurde - Nachrichten Politik - Ausland - DIE WELT&quot;</span>","value":"\"Ferguson : Der Tag, an dem die US-Polizei mein Feind wurde - Nachrichten Politik - Ausland - DIE WELT\""}
;; <=

;; @@
(-> some-tree :analysis keys)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(:size :height :users :pub-times :hashtags :delays)</span>","value":"(:size :height :users :pub-times :hashtags :delays)"}
;; <=

;; **
;;; ### Basic Tree Metrics
;; **

;; @@
(-> some-tree :analysis (dissoc :pub-times :hashtags :delays :users))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:size</span>","value":":size"},{"type":"html","content":"<span class='clj-long'>479</span>","value":"479"}],"value":"[:size 479]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:height</span>","value":":height"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[:height 5]"}],"value":"{:size 479, :height 5}"}
;; <=

;; **
;;; ### Hashtag Distribution
;;; Top 10 hashtags.
;; **

;; @@
(let [hashtags (-> some-tree :analysis :hashtags)]
  (if (empty? hashtags)
    "No hashtags"
    (let [h-freq (->> hashtags frequencies (sort-by second >) (take 10))]
  [(into #{} hashtags)
   (bar-chart (keys h-freq) (vals h-freq) :plot-size 800)])))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;pressefreiheit&quot;</span>","value":"\"pressefreiheit\""},{"type":"html","content":"<span class='clj-string'>&quot;us&quot;</span>","value":"\"us\""},{"type":"html","content":"<span class='clj-string'>&quot;imgeilstenlandderwelt&quot;</span>","value":"\"imgeilstenlandderwelt\""},{"type":"html","content":"<span class='clj-string'>&quot;polizei&quot;</span>","value":"\"polizei\""},{"type":"html","content":"<span class='clj-string'>&quot;springer&quot;</span>","value":"\"springer\""},{"type":"html","content":"<span class='clj-string'>&quot;furgeson&quot;</span>","value":"\"furgeson\""},{"type":"html","content":"<span class='clj-string'>&quot;usa&quot;</span>","value":"\"usa\""},{"type":"html","content":"<span class='clj-string'>&quot;s21&quot;</span>","value":"\"s21\""},{"type":"html","content":"<span class='clj-string'>&quot;graw&quot;</span>","value":"\"graw\""},{"type":"html","content":"<span class='clj-string'>&quot;medien&quot;</span>","value":"\"medien\""},{"type":"html","content":"<span class='clj-string'>&quot;missouri&quot;</span>","value":"\"missouri\""},{"type":"html","content":"<span class='clj-string'>&quot;schland&quot;</span>","value":"\"schland\""},{"type":"html","content":"<span class='clj-string'>&quot;policestate&quot;</span>","value":"\"policestate\""},{"type":"html","content":"<span class='clj-string'>&quot;arbeitsvertrag&quot;</span>","value":"\"arbeitsvertrag\""},{"type":"html","content":"<span class='clj-string'>&quot;firstamendment&quot;</span>","value":"\"firstamendment\""},{"type":"html","content":"<span class='clj-string'>&quot;ferguson&quot;</span>","value":"\"ferguson\""},{"type":"html","content":"<span class='clj-string'>&quot;afd&quot;</span>","value":"\"afd\""},{"type":"html","content":"<span class='clj-string'>&quot;atlantik&quot;</span>","value":"\"atlantik\""}],"value":"#{\"pressefreiheit\" \"us\" \"imgeilstenlandderwelt\" \"polizei\" \"springer\" \"furgeson\" \"usa\" \"s21\" \"graw\" \"medien\" \"missouri\" \"schland\" \"policestate\" \"arbeitsvertrag\" \"firstamendment\" \"ferguson\" \"afd\" \"atlantik\"}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"d4d74364-fe6f-421c-b8a1-4281749a341b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"d4d74364-fe6f-421c-b8a1-4281749a341b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"d4d74364-fe6f-421c-b8a1-4281749a341b"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"d4d74364-fe6f-421c-b8a1-4281749a341b","values":[{"x":"ferguson","y":124},{"x":"usa","y":5},{"x":"pressefreiheit","y":2},{"x":"us","y":2},{"x":"springer","y":2},{"x":"furgeson","y":2},{"x":"s21","y":2},{"x":"graw","y":2},{"x":"policestate","y":2},{"x":"arbeitsvertrag","y":2}]}],"width":800,"height":494.4375915527344,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"d4d74364-fe6f-421c-b8a1-4281749a341b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"d4d74364-fe6f-421c-b8a1-4281749a341b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"d4d74364-fe6f-421c-b8a1-4281749a341b\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"d4d74364-fe6f-421c-b8a1-4281749a341b\", :values ({:x \"ferguson\", :y 124} {:x \"usa\", :y 5} {:x \"pressefreiheit\", :y 2} {:x \"us\", :y 2} {:x \"springer\", :y 2} {:x \"furgeson\", :y 2} {:x \"s21\", :y 2} {:x \"graw\", :y 2} {:x \"policestate\", :y 2} {:x \"arbeitsvertrag\", :y 2})}], :width 800, :height 494.4376, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[#{\"pressefreiheit\" \"us\" \"imgeilstenlandderwelt\" \"polizei\" \"springer\" \"furgeson\" \"usa\" \"s21\" \"graw\" \"medien\" \"missouri\" \"schland\" \"policestate\" \"arbeitsvertrag\" \"firstamendment\" \"ferguson\" \"afd\" \"atlantik\"} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"d4d74364-fe6f-421c-b8a1-4281749a341b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"d4d74364-fe6f-421c-b8a1-4281749a341b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"d4d74364-fe6f-421c-b8a1-4281749a341b\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"d4d74364-fe6f-421c-b8a1-4281749a341b\", :values ({:x \"ferguson\", :y 124} {:x \"usa\", :y 5} {:x \"pressefreiheit\", :y 2} {:x \"us\", :y 2} {:x \"springer\", :y 2} {:x \"furgeson\", :y 2} {:x \"s21\", :y 2} {:x \"graw\", :y 2} {:x \"policestate\", :y 2} {:x \"arbeitsvertrag\", :y 2})}], :width 800, :height 494.4376, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;; <=

;; **
;;; ### User Post Distribution
;;; Top 10 Users related to this article.
;; **

;; @@
(let [users (->> some-tree :analysis :users frequencies (sort-by second >) (take 10))]
  (bar-chart (keys users) (vals users) :plot-size 800))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"8e0c572e-7575-4bd8-824a-5ed4636a2343","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"8e0c572e-7575-4bd8-824a-5ed4636a2343","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"8e0c572e-7575-4bd8-824a-5ed4636a2343"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"8e0c572e-7575-4bd8-824a-5ed4636a2343","values":[{"x":"welt","y":10},{"x":"AufstandS21","y":6},{"x":"olaf26","y":3},{"x":"Welatpiraz","y":3},{"x":"frisch_fish","y":3},{"x":"rapic2012","y":3},{"x":"jep_","y":3},{"x":"Ibicis","y":3},{"x":"thetruemoeckert","y":3},{"x":"LakehillSoccer","y":3}]}],"width":800,"height":494.4375915527344,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e0c572e-7575-4bd8-824a-5ed4636a2343\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e0c572e-7575-4bd8-824a-5ed4636a2343\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"8e0c572e-7575-4bd8-824a-5ed4636a2343\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"8e0c572e-7575-4bd8-824a-5ed4636a2343\", :values ({:x \"welt\", :y 10} {:x \"AufstandS21\", :y 6} {:x \"olaf26\", :y 3} {:x \"Welatpiraz\", :y 3} {:x \"frisch_fish\", :y 3} {:x \"rapic2012\", :y 3} {:x \"jep_\", :y 3} {:x \"Ibicis\", :y 3} {:x \"thetruemoeckert\", :y 3} {:x \"LakehillSoccer\", :y 3})}], :width 800, :height 494.4376, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ### Delay Distribution
;;; Reaction's delay of an article in minutes.
;; **

;; @@
(let [delays (->> some-tree :analysis :delays (map #(float (/ % 60))))]
  (histogram delays :plot-size 900))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"a2e7b181-1f0c-44d0-912d-e03beb39afae","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"a2e7b181-1f0c-44d0-912d-e03beb39afae","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"a2e7b181-1f0c-44d0-912d-e03beb39afae"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"a2e7b181-1f0c-44d0-912d-e03beb39afae","values":[{"x":0.0,"y":0},{"x":1888.1066406250004,"y":461.0},{"x":3776.213281250001,"y":6.0},{"x":5664.3199218750015,"y":6.0},{"x":7552.426562500002,"y":3.0},{"x":9440.533203125002,"y":0.0},{"x":11328.639843750003,"y":0.0},{"x":13216.746484375004,"y":2.0},{"x":15104.853125000005,"y":0.0},{"x":16992.959765625004,"y":0.0},{"x":18881.066406250004,"y":1.0},{"x":20769.173046875003,"y":0}]}],"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"a2e7b181-1f0c-44d0-912d-e03beb39afae\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"a2e7b181-1f0c-44d0-912d-e03beb39afae\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"a2e7b181-1f0c-44d0-912d-e03beb39afae\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"a2e7b181-1f0c-44d0-912d-e03beb39afae\", :values ({:x 0.0, :y 0} {:x 1888.1066406250004, :y 461.0} {:x 3776.213281250001, :y 6.0} {:x 5664.3199218750015, :y 6.0} {:x 7552.426562500002, :y 3.0} {:x 9440.533203125002, :y 0.0} {:x 11328.639843750003, :y 0.0} {:x 13216.746484375004, :y 2.0} {:x 15104.853125000005, :y 0.0} {:x 16992.959765625004, :y 0.0} {:x 18881.066406250004, :y 1.0} {:x 20769.173046875003, :y 0})}], :width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ### Example Summary
;; **

;; @@
(def dpa-summary (compute-summary "dpa"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/dpa-summary</span>","value":"#'unsightly-reserve/dpa-summary"}
;; <=

;; @@
(keys dpa-summary)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(:source :article-count :analytics :total-impact :avg-impact :no-reactions :avg-height)</span>","value":"(:source :article-count :analytics :total-impact :avg-impact :no-reactions :avg-height)"}
;; <=

;; @@
(dissoc dpa-summary :analytics :pub-times)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"},{"type":"html","content":"<span class='clj-string'>&quot;dpa&quot;</span>","value":"\"dpa\""}],"value":"[:source \"dpa\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:article-count</span>","value":":article-count"},{"type":"html","content":"<span class='clj-unkown'>965</span>","value":"965"}],"value":"[:article-count 965]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:total-impact</span>","value":":total-impact"},{"type":"html","content":"<span class='clj-long'>4811</span>","value":"4811"}],"value":"[:total-impact 4811]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:avg-impact</span>","value":":avg-impact"},{"type":"html","content":"<span class='clj-unkown'>4.985492</span>","value":"4.985492"}],"value":"[:avg-impact 4.985492]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:no-reactions</span>","value":":no-reactions"},{"type":"html","content":"<span class='clj-unkown'>0.19170985</span>","value":"0.19170985"}],"value":"[:no-reactions 0.19170985]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:avg-height</span>","value":":avg-height"},{"type":"html","content":"<span class='clj-unkown'>0.98031086</span>","value":"0.98031086"}],"value":"[:avg-height 0.98031086]"}],"value":"{:source \"dpa\", :article-count 965, :total-impact 4811, :avg-impact 4.985492, :no-reactions 0.19170985, :avg-height 0.98031086}"}
;; <=

;; @@
(-> dpa-summary :analytics first keys)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(:size :height :users :pub-times :hashtags :delays)</span>","value":"(:size :height :users :pub-times :hashtags :delays)"}
;; <=

;; @@
(let [sizes (->> dpa-summary :analytics (map :size))]
  (histogram sizes :plot-size 900 :bins 100))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"69d6f730-97cd-462b-8ec5-6e1cabd31353","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"69d6f730-97cd-462b-8ec5-6e1cabd31353","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"69d6f730-97cd-462b-8ec5-6e1cabd31353"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"69d6f730-97cd-462b-8ec5-6e1cabd31353","values":[{"x":1.0,"y":0},{"x":1.56,"y":185.0},{"x":2.12,"y":112.0},{"x":2.68,"y":0.0},{"x":3.24,"y":149.0},{"x":3.8000000000000003,"y":0.0},{"x":4.36,"y":112.0},{"x":4.92,"y":0.0},{"x":5.48,"y":101.0},{"x":6.040000000000001,"y":76.0},{"x":6.600000000000001,"y":0.0},{"x":7.160000000000002,"y":48.0},{"x":7.720000000000002,"y":0.0},{"x":8.280000000000003,"y":42.0},{"x":8.840000000000003,"y":0.0},{"x":9.400000000000004,"y":40.0},{"x":9.960000000000004,"y":0.0},{"x":10.520000000000005,"y":24.0},{"x":11.080000000000005,"y":20.0},{"x":11.640000000000006,"y":0.0},{"x":12.200000000000006,"y":7.0},{"x":12.760000000000007,"y":0.0},{"x":13.320000000000007,"y":3.0},{"x":13.880000000000008,"y":0.0},{"x":14.440000000000008,"y":9.0},{"x":15.000000000000009,"y":5.0},{"x":15.56000000000001,"y":0.0},{"x":16.120000000000008,"y":4.0},{"x":16.680000000000007,"y":0.0},{"x":17.240000000000006,"y":3.0},{"x":17.800000000000004,"y":0.0},{"x":18.360000000000003,"y":6.0},{"x":18.92,"y":0.0},{"x":19.48,"y":3.0},{"x":20.04,"y":1.0},{"x":20.599999999999998,"y":0.0},{"x":21.159999999999997,"y":2.0},{"x":21.719999999999995,"y":0.0},{"x":22.279999999999994,"y":2.0},{"x":22.839999999999993,"y":0.0},{"x":23.39999999999999,"y":6.0},{"x":23.95999999999999,"y":0.0},{"x":24.51999999999999,"y":0.0},{"x":25.079999999999988,"y":0.0},{"x":25.639999999999986,"y":0.0},{"x":26.199999999999985,"y":1.0},{"x":26.759999999999984,"y":0.0},{"x":27.319999999999983,"y":1.0},{"x":27.87999999999998,"y":0.0},{"x":28.43999999999998,"y":0.0},{"x":28.99999999999998,"y":0.0},{"x":29.559999999999977,"y":0.0},{"x":30.119999999999976,"y":0.0},{"x":30.679999999999975,"y":0.0},{"x":31.239999999999974,"y":0.0},{"x":31.799999999999972,"y":0.0},{"x":32.35999999999997,"y":0.0},{"x":32.91999999999997,"y":0.0},{"x":33.479999999999976,"y":0.0},{"x":34.03999999999998,"y":0.0},{"x":34.59999999999998,"y":0.0},{"x":35.15999999999998,"y":0.0},{"x":35.719999999999985,"y":0.0},{"x":36.27999999999999,"y":0.0},{"x":36.83999999999999,"y":0.0},{"x":37.39999999999999,"y":0.0},{"x":37.959999999999994,"y":0.0},{"x":38.519999999999996,"y":0.0},{"x":39.08,"y":0.0},{"x":39.64,"y":0.0},{"x":40.2,"y":1.0},{"x":40.760000000000005,"y":0.0},{"x":41.32000000000001,"y":0.0},{"x":41.88000000000001,"y":0.0},{"x":42.44000000000001,"y":0.0},{"x":43.000000000000014,"y":0.0},{"x":43.56000000000002,"y":0.0},{"x":44.12000000000002,"y":0.0},{"x":44.68000000000002,"y":0.0},{"x":45.24000000000002,"y":0.0},{"x":45.800000000000026,"y":0.0},{"x":46.36000000000003,"y":0.0},{"x":46.92000000000003,"y":0.0},{"x":47.48000000000003,"y":0.0},{"x":48.040000000000035,"y":0.0},{"x":48.60000000000004,"y":0.0},{"x":49.16000000000004,"y":0.0},{"x":49.72000000000004,"y":0.0},{"x":50.280000000000044,"y":0.0},{"x":50.840000000000046,"y":0.0},{"x":51.40000000000005,"y":0.0},{"x":51.96000000000005,"y":0.0},{"x":52.52000000000005,"y":0.0},{"x":53.080000000000055,"y":0.0},{"x":53.64000000000006,"y":0.0},{"x":54.20000000000006,"y":0.0},{"x":54.76000000000006,"y":0.0},{"x":55.320000000000064,"y":0.0},{"x":55.88000000000007,"y":0.0},{"x":56.44000000000007,"y":1.0},{"x":57.00000000000007,"y":1.0},{"x":57.56000000000007,"y":0}]}],"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"69d6f730-97cd-462b-8ec5-6e1cabd31353\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"69d6f730-97cd-462b-8ec5-6e1cabd31353\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"69d6f730-97cd-462b-8ec5-6e1cabd31353\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"69d6f730-97cd-462b-8ec5-6e1cabd31353\", :values ({:x 1.0, :y 0} {:x 1.56, :y 185.0} {:x 2.12, :y 112.0} {:x 2.68, :y 0.0} {:x 3.24, :y 149.0} {:x 3.8000000000000003, :y 0.0} {:x 4.36, :y 112.0} {:x 4.92, :y 0.0} {:x 5.48, :y 101.0} {:x 6.040000000000001, :y 76.0} {:x 6.600000000000001, :y 0.0} {:x 7.160000000000002, :y 48.0} {:x 7.720000000000002, :y 0.0} {:x 8.280000000000003, :y 42.0} {:x 8.840000000000003, :y 0.0} {:x 9.400000000000004, :y 40.0} {:x 9.960000000000004, :y 0.0} {:x 10.520000000000005, :y 24.0} {:x 11.080000000000005, :y 20.0} {:x 11.640000000000006, :y 0.0} {:x 12.200000000000006, :y 7.0} {:x 12.760000000000007, :y 0.0} {:x 13.320000000000007, :y 3.0} {:x 13.880000000000008, :y 0.0} {:x 14.440000000000008, :y 9.0} {:x 15.000000000000009, :y 5.0} {:x 15.56000000000001, :y 0.0} {:x 16.120000000000008, :y 4.0} {:x 16.680000000000007, :y 0.0} {:x 17.240000000000006, :y 3.0} {:x 17.800000000000004, :y 0.0} {:x 18.360000000000003, :y 6.0} {:x 18.92, :y 0.0} {:x 19.48, :y 3.0} {:x 20.04, :y 1.0} {:x 20.599999999999998, :y 0.0} {:x 21.159999999999997, :y 2.0} {:x 21.719999999999995, :y 0.0} {:x 22.279999999999994, :y 2.0} {:x 22.839999999999993, :y 0.0} {:x 23.39999999999999, :y 6.0} {:x 23.95999999999999, :y 0.0} {:x 24.51999999999999, :y 0.0} {:x 25.079999999999988, :y 0.0} {:x 25.639999999999986, :y 0.0} {:x 26.199999999999985, :y 1.0} {:x 26.759999999999984, :y 0.0} {:x 27.319999999999983, :y 1.0} {:x 27.87999999999998, :y 0.0} {:x 28.43999999999998, :y 0.0} {:x 28.99999999999998, :y 0.0} {:x 29.559999999999977, :y 0.0} {:x 30.119999999999976, :y 0.0} {:x 30.679999999999975, :y 0.0} {:x 31.239999999999974, :y 0.0} {:x 31.799999999999972, :y 0.0} {:x 32.35999999999997, :y 0.0} {:x 32.91999999999997, :y 0.0} {:x 33.479999999999976, :y 0.0} {:x 34.03999999999998, :y 0.0} {:x 34.59999999999998, :y 0.0} {:x 35.15999999999998, :y 0.0} {:x 35.719999999999985, :y 0.0} {:x 36.27999999999999, :y 0.0} {:x 36.83999999999999, :y 0.0} {:x 37.39999999999999, :y 0.0} {:x 37.959999999999994, :y 0.0} {:x 38.519999999999996, :y 0.0} {:x 39.08, :y 0.0} {:x 39.64, :y 0.0} {:x 40.2, :y 1.0} {:x 40.760000000000005, :y 0.0} {:x 41.32000000000001, :y 0.0} {:x 41.88000000000001, :y 0.0} {:x 42.44000000000001, :y 0.0} {:x 43.000000000000014, :y 0.0} {:x 43.56000000000002, :y 0.0} {:x 44.12000000000002, :y 0.0} {:x 44.68000000000002, :y 0.0} {:x 45.24000000000002, :y 0.0} {:x 45.800000000000026, :y 0.0} {:x 46.36000000000003, :y 0.0} {:x 46.92000000000003, :y 0.0} {:x 47.48000000000003, :y 0.0} {:x 48.040000000000035, :y 0.0} {:x 48.60000000000004, :y 0.0} {:x 49.16000000000004, :y 0.0} {:x 49.72000000000004, :y 0.0} {:x 50.280000000000044, :y 0.0} {:x 50.840000000000046, :y 0.0} {:x 51.40000000000005, :y 0.0} {:x 51.96000000000005, :y 0.0} {:x 52.52000000000005, :y 0.0} {:x 53.080000000000055, :y 0.0} {:x 53.64000000000006, :y 0.0} {:x 54.20000000000006, :y 0.0} {:x 54.76000000000006, :y 0.0} {:x 55.320000000000064, :y 0.0} {:x 55.88000000000007, :y 0.0} {:x 56.44000000000007, :y 1.0} {:x 57.00000000000007, :y 1.0} {:x 57.56000000000007, :y 0})}], :width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(let [delays (->> dpa-summary 
                  :analytics 
                  (map :delays) 
                  (map (fn [ds] (/ (reduce + ds) (count ds) 60)))
                  (remove #(> % 1440)))]
  (histogram delays :plot-size 900 :bins 100))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"12db4ced-ee32-4b85-8ae0-7ebe1c800da5","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"12db4ced-ee32-4b85-8ae0-7ebe1c800da5","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"12db4ced-ee32-4b85-8ae0-7ebe1c800da5"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"12db4ced-ee32-4b85-8ae0-7ebe1c800da5","values":[{"x":0.0,"y":0},{"x":13.507833333333332,"y":477.0},{"x":27.015666666666664,"y":119.0},{"x":40.5235,"y":80.0},{"x":54.03133333333333,"y":53.0},{"x":67.53916666666666,"y":35.0},{"x":81.047,"y":31.0},{"x":94.55483333333333,"y":25.0},{"x":108.06266666666667,"y":20.0},{"x":121.57050000000001,"y":15.0},{"x":135.07833333333335,"y":15.0},{"x":148.58616666666668,"y":7.0},{"x":162.09400000000002,"y":6.0},{"x":175.60183333333336,"y":7.0},{"x":189.1096666666667,"y":3.0},{"x":202.61750000000004,"y":8.0},{"x":216.12533333333337,"y":3.0},{"x":229.6331666666667,"y":4.0},{"x":243.14100000000005,"y":2.0},{"x":256.64883333333336,"y":3.0},{"x":270.1566666666667,"y":1.0},{"x":283.66450000000003,"y":2.0},{"x":297.17233333333337,"y":2.0},{"x":310.6801666666667,"y":2.0},{"x":324.18800000000005,"y":0.0},{"x":337.6958333333334,"y":1.0},{"x":351.2036666666667,"y":1.0},{"x":364.71150000000006,"y":0.0},{"x":378.2193333333334,"y":3.0},{"x":391.72716666666673,"y":0.0},{"x":405.23500000000007,"y":0.0},{"x":418.7428333333334,"y":4.0},{"x":432.25066666666675,"y":0.0},{"x":445.7585000000001,"y":0.0},{"x":459.2663333333334,"y":0.0},{"x":472.77416666666676,"y":1.0},{"x":486.2820000000001,"y":1.0},{"x":499.78983333333343,"y":1.0},{"x":513.2976666666667,"y":0.0},{"x":526.8055,"y":0.0},{"x":540.3133333333334,"y":2.0},{"x":553.8211666666667,"y":0.0},{"x":567.3290000000001,"y":1.0},{"x":580.8368333333334,"y":0.0},{"x":594.3446666666667,"y":0.0},{"x":607.8525000000001,"y":3.0},{"x":621.3603333333334,"y":0.0},{"x":634.8681666666668,"y":1.0},{"x":648.3760000000001,"y":0.0},{"x":661.8838333333334,"y":0.0},{"x":675.3916666666668,"y":1.0},{"x":688.8995000000001,"y":0.0},{"x":702.4073333333334,"y":1.0},{"x":715.9151666666668,"y":0.0},{"x":729.4230000000001,"y":0.0},{"x":742.9308333333335,"y":1.0},{"x":756.4386666666668,"y":0.0},{"x":769.9465000000001,"y":2.0},{"x":783.4543333333335,"y":0.0},{"x":796.9621666666668,"y":0.0},{"x":810.4700000000001,"y":0.0},{"x":823.9778333333335,"y":1.0},{"x":837.4856666666668,"y":0.0},{"x":850.9935000000002,"y":1.0},{"x":864.5013333333335,"y":0.0},{"x":878.0091666666668,"y":0.0},{"x":891.5170000000002,"y":0.0},{"x":905.0248333333335,"y":0.0},{"x":918.5326666666668,"y":0.0},{"x":932.0405000000002,"y":1.0},{"x":945.5483333333335,"y":0.0},{"x":959.0561666666669,"y":1.0},{"x":972.5640000000002,"y":0.0},{"x":986.0718333333335,"y":1.0},{"x":999.5796666666669,"y":1.0},{"x":1013.0875000000002,"y":0.0},{"x":1026.5953333333334,"y":2.0},{"x":1040.1031666666668,"y":0.0},{"x":1053.611,"y":0.0},{"x":1067.1188333333334,"y":0.0},{"x":1080.6266666666668,"y":0.0},{"x":1094.1345000000001,"y":0.0},{"x":1107.6423333333335,"y":0.0},{"x":1121.1501666666668,"y":0.0},{"x":1134.6580000000001,"y":0.0},{"x":1148.1658333333335,"y":0.0},{"x":1161.6736666666668,"y":1.0},{"x":1175.1815000000001,"y":0.0},{"x":1188.6893333333335,"y":1.0},{"x":1202.1971666666668,"y":0.0},{"x":1215.7050000000002,"y":1.0},{"x":1229.2128333333335,"y":0.0},{"x":1242.7206666666668,"y":0.0},{"x":1256.2285000000002,"y":0.0},{"x":1269.7363333333335,"y":0.0},{"x":1283.2441666666668,"y":0.0},{"x":1296.7520000000002,"y":0.0},{"x":1310.2598333333335,"y":0.0},{"x":1323.7676666666669,"y":0.0},{"x":1337.2755000000002,"y":0.0},{"x":1350.7833333333335,"y":1.0},{"x":1364.2911666666669,"y":0}]}],"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"12db4ced-ee32-4b85-8ae0-7ebe1c800da5\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"12db4ced-ee32-4b85-8ae0-7ebe1c800da5\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"12db4ced-ee32-4b85-8ae0-7ebe1c800da5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"12db4ced-ee32-4b85-8ae0-7ebe1c800da5\", :values ({:x 0.0, :y 0} {:x 13.507833333333332, :y 477.0} {:x 27.015666666666664, :y 119.0} {:x 40.5235, :y 80.0} {:x 54.03133333333333, :y 53.0} {:x 67.53916666666666, :y 35.0} {:x 81.047, :y 31.0} {:x 94.55483333333333, :y 25.0} {:x 108.06266666666667, :y 20.0} {:x 121.57050000000001, :y 15.0} {:x 135.07833333333335, :y 15.0} {:x 148.58616666666668, :y 7.0} {:x 162.09400000000002, :y 6.0} {:x 175.60183333333336, :y 7.0} {:x 189.1096666666667, :y 3.0} {:x 202.61750000000004, :y 8.0} {:x 216.12533333333337, :y 3.0} {:x 229.6331666666667, :y 4.0} {:x 243.14100000000005, :y 2.0} {:x 256.64883333333336, :y 3.0} {:x 270.1566666666667, :y 1.0} {:x 283.66450000000003, :y 2.0} {:x 297.17233333333337, :y 2.0} {:x 310.6801666666667, :y 2.0} {:x 324.18800000000005, :y 0.0} {:x 337.6958333333334, :y 1.0} {:x 351.2036666666667, :y 1.0} {:x 364.71150000000006, :y 0.0} {:x 378.2193333333334, :y 3.0} {:x 391.72716666666673, :y 0.0} {:x 405.23500000000007, :y 0.0} {:x 418.7428333333334, :y 4.0} {:x 432.25066666666675, :y 0.0} {:x 445.7585000000001, :y 0.0} {:x 459.2663333333334, :y 0.0} {:x 472.77416666666676, :y 1.0} {:x 486.2820000000001, :y 1.0} {:x 499.78983333333343, :y 1.0} {:x 513.2976666666667, :y 0.0} {:x 526.8055, :y 0.0} {:x 540.3133333333334, :y 2.0} {:x 553.8211666666667, :y 0.0} {:x 567.3290000000001, :y 1.0} {:x 580.8368333333334, :y 0.0} {:x 594.3446666666667, :y 0.0} {:x 607.8525000000001, :y 3.0} {:x 621.3603333333334, :y 0.0} {:x 634.8681666666668, :y 1.0} {:x 648.3760000000001, :y 0.0} {:x 661.8838333333334, :y 0.0} {:x 675.3916666666668, :y 1.0} {:x 688.8995000000001, :y 0.0} {:x 702.4073333333334, :y 1.0} {:x 715.9151666666668, :y 0.0} {:x 729.4230000000001, :y 0.0} {:x 742.9308333333335, :y 1.0} {:x 756.4386666666668, :y 0.0} {:x 769.9465000000001, :y 2.0} {:x 783.4543333333335, :y 0.0} {:x 796.9621666666668, :y 0.0} {:x 810.4700000000001, :y 0.0} {:x 823.9778333333335, :y 1.0} {:x 837.4856666666668, :y 0.0} {:x 850.9935000000002, :y 1.0} {:x 864.5013333333335, :y 0.0} {:x 878.0091666666668, :y 0.0} {:x 891.5170000000002, :y 0.0} {:x 905.0248333333335, :y 0.0} {:x 918.5326666666668, :y 0.0} {:x 932.0405000000002, :y 1.0} {:x 945.5483333333335, :y 0.0} {:x 959.0561666666669, :y 1.0} {:x 972.5640000000002, :y 0.0} {:x 986.0718333333335, :y 1.0} {:x 999.5796666666669, :y 1.0} {:x 1013.0875000000002, :y 0.0} {:x 1026.5953333333334, :y 2.0} {:x 1040.1031666666668, :y 0.0} {:x 1053.611, :y 0.0} {:x 1067.1188333333334, :y 0.0} {:x 1080.6266666666668, :y 0.0} {:x 1094.1345000000001, :y 0.0} {:x 1107.6423333333335, :y 0.0} {:x 1121.1501666666668, :y 0.0} {:x 1134.6580000000001, :y 0.0} {:x 1148.1658333333335, :y 0.0} {:x 1161.6736666666668, :y 1.0} {:x 1175.1815000000001, :y 0.0} {:x 1188.6893333333335, :y 1.0} {:x 1202.1971666666668, :y 0.0} {:x 1215.7050000000002, :y 1.0} {:x 1229.2128333333335, :y 0.0} {:x 1242.7206666666668, :y 0.0} {:x 1256.2285000000002, :y 0.0} {:x 1269.7363333333335, :y 0.0} {:x 1283.2441666666668, :y 0.0} {:x 1296.7520000000002, :y 0.0} {:x 1310.2598333333335, :y 0.0} {:x 1323.7676666666669, :y 0.0} {:x 1337.2755000000002, :y 0.0} {:x 1350.7833333333335, :y 1.0} {:x 1364.2911666666669, :y 0})}], :width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(let [heights (->> dpa-summary :analytics (map :height))]
  (histogram heights :plot-size 900 :bins 10))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e","values":[{"x":0.0,"y":0},{"x":0.7000000000000001,"y":185.0},{"x":1.4000000000000001,"y":661.0},{"x":2.1,"y":92.0},{"x":2.8000000000000003,"y":0.0},{"x":3.5000000000000004,"y":15.0},{"x":4.2,"y":8.0},{"x":4.9,"y":0.0},{"x":5.6000000000000005,"y":1.0},{"x":6.300000000000001,"y":2.0},{"x":7.000000000000001,"y":1.0},{"x":7.700000000000001,"y":0}]}],"width":900,"height":556.2422485351562,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"26d75bbd-1c8a-4322-8af8-d0cdeb515d2e\", :values ({:x 0.0, :y 0} {:x 0.7000000000000001, :y 185.0} {:x 1.4000000000000001, :y 661.0} {:x 2.1, :y 92.0} {:x 2.8000000000000003, :y 0.0} {:x 3.5000000000000004, :y 15.0} {:x 4.2, :y 8.0} {:x 4.9, :y 0.0} {:x 5.6000000000000005, :y 1.0} {:x 6.300000000000001, :y 2.0} {:x 7.000000000000001, :y 1.0} {:x 7.700000000000001, :y 0})}], :width 900, :height 556.24225, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(let [hashtags (->> dpa-summary 
                  :analytics 
                  (map :hashtags) 
                  (apply concat)
                  (remove #{"dpa" "dpavolo"})
                  frequencies 
                  (sort-by second >) 
                  (take 20))]
  [(keys hashtags)
  (bar-chart (keys hashtags) (vals hashtags) :plot-size 1000)])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>(&quot;ebola&quot; &quot;is&quot; &quot;sltw14&quot; &quot;wowereit&quot; &quot;ferguson&quot; &quot;snowden&quot; &quot;hongkong&quot; &quot;nsu&quot; &quot;uber&quot; &quot;afd&quot; &quot;indyref&quot; &quot;mh17&quot; &quot;china&quot; &quot;icebucketchallenge&quot; &quot;apple&quot; &quot;nsa&quot; &quot;news&quot; &quot;n24&quot; &quot;piraten&quot; &quot;tohti&quot;)</span>","value":"(\"ebola\" \"is\" \"sltw14\" \"wowereit\" \"ferguson\" \"snowden\" \"hongkong\" \"nsu\" \"uber\" \"afd\" \"indyref\" \"mh17\" \"china\" \"icebucketchallenge\" \"apple\" \"nsa\" \"news\" \"n24\" \"piraten\" \"tohti\")"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"90c80f82-240c-4c0c-ab4c-a783e2a3b731","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"90c80f82-240c-4c0c-ab4c-a783e2a3b731","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"90c80f82-240c-4c0c-ab4c-a783e2a3b731"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"90c80f82-240c-4c0c-ab4c-a783e2a3b731","values":[{"x":"ebola","y":47},{"x":"is","y":45},{"x":"sltw14","y":34},{"x":"wowereit","y":33},{"x":"ferguson","y":30},{"x":"snowden","y":27},{"x":"hongkong","y":25},{"x":"nsu","y":22},{"x":"uber","y":20},{"x":"afd","y":19},{"x":"indyref","y":18},{"x":"mh17","y":17},{"x":"china","y":17},{"x":"icebucketchallenge","y":16},{"x":"apple","y":15},{"x":"nsa","y":15},{"x":"news","y":14},{"x":"n24","y":14},{"x":"piraten","y":14},{"x":"tohti","y":13}]}],"width":1000,"height":618.0469970703125,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\", :values ({:x \"ebola\", :y 47} {:x \"is\", :y 45} {:x \"sltw14\", :y 34} {:x \"wowereit\", :y 33} {:x \"ferguson\", :y 30} {:x \"snowden\", :y 27} {:x \"hongkong\", :y 25} {:x \"nsu\", :y 22} {:x \"uber\", :y 20} {:x \"afd\", :y 19} {:x \"indyref\", :y 18} {:x \"mh17\", :y 17} {:x \"china\", :y 17} {:x \"icebucketchallenge\", :y 16} {:x \"apple\", :y 15} {:x \"nsa\", :y 15} {:x \"news\", :y 14} {:x \"n24\", :y 14} {:x \"piraten\", :y 14} {:x \"tohti\", :y 13})}], :width 1000, :height 618.047, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[(\"ebola\" \"is\" \"sltw14\" \"wowereit\" \"ferguson\" \"snowden\" \"hongkong\" \"nsu\" \"uber\" \"afd\" \"indyref\" \"mh17\" \"china\" \"icebucketchallenge\" \"apple\" \"nsa\" \"news\" \"n24\" \"piraten\" \"tohti\") #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"90c80f82-240c-4c0c-ab4c-a783e2a3b731\", :values ({:x \"ebola\", :y 47} {:x \"is\", :y 45} {:x \"sltw14\", :y 34} {:x \"wowereit\", :y 33} {:x \"ferguson\", :y 30} {:x \"snowden\", :y 27} {:x \"hongkong\", :y 25} {:x \"nsu\", :y 22} {:x \"uber\", :y 20} {:x \"afd\", :y 19} {:x \"indyref\", :y 18} {:x \"mh17\", :y 17} {:x \"china\", :y 17} {:x \"icebucketchallenge\", :y 16} {:x \"apple\", :y 15} {:x \"nsa\", :y 15} {:x \"news\", :y 14} {:x \"n24\", :y 14} {:x \"piraten\", :y 14} {:x \"tohti\", :y 13})}], :width 1000, :height 618.047, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;; <=

;; @@
(def dpa-users (->> dpa-summary 
                  :analytics 
                  (map :users)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/dpa-users</span>","value":"#'unsightly-reserve/dpa-users"}
;; <=

;; @@
(let [users (->> dpa-users
                 (apply concat)
                 (remove #{"dpa"})
                 frequencies 
                 (sort-by second >) 
                 (take 15))]
  [(keys users)
  (bar-chart (keys users) (vals users) :plot-size 1000)])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>(&quot;Dietmar_Bauer&quot; &quot;alanyanews_eu&quot; &quot;thesirenofangel&quot; &quot;grimneko&quot; &quot;2014FL&quot; &quot;BBattmer&quot; &quot;S_Tagblatt&quot; &quot;Klostifan1981&quot; &quot;ChrisBrey1&quot; &quot;DietrichOpal&quot; &quot;martinromanczyk&quot; &quot;Dennis_Snehotta&quot; &quot;Liane13&quot; &quot;andreaslandwehr&quot; &quot;WOLKEFN&quot;)</span>","value":"(\"Dietmar_Bauer\" \"alanyanews_eu\" \"thesirenofangel\" \"grimneko\" \"2014FL\" \"BBattmer\" \"S_Tagblatt\" \"Klostifan1981\" \"ChrisBrey1\" \"DietrichOpal\" \"martinromanczyk\" \"Dennis_Snehotta\" \"Liane13\" \"andreaslandwehr\" \"WOLKEFN\")"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"1a0f4889-685a-4e77-8947-83dcaa838d4b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"1a0f4889-685a-4e77-8947-83dcaa838d4b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"1a0f4889-685a-4e77-8947-83dcaa838d4b"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"1a0f4889-685a-4e77-8947-83dcaa838d4b","values":[{"x":"Dietmar_Bauer","y":120},{"x":"alanyanews_eu","y":55},{"x":"thesirenofangel","y":47},{"x":"grimneko","y":32},{"x":"2014FL","y":30},{"x":"BBattmer","y":30},{"x":"S_Tagblatt","y":29},{"x":"Klostifan1981","y":26},{"x":"ChrisBrey1","y":24},{"x":"DietrichOpal","y":23},{"x":"martinromanczyk","y":22},{"x":"Dennis_Snehotta","y":21},{"x":"Liane13","y":20},{"x":"andreaslandwehr","y":19},{"x":"WOLKEFN","y":19}]}],"width":1000,"height":618.0469970703125,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"1a0f4889-685a-4e77-8947-83dcaa838d4b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"1a0f4889-685a-4e77-8947-83dcaa838d4b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"1a0f4889-685a-4e77-8947-83dcaa838d4b\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"1a0f4889-685a-4e77-8947-83dcaa838d4b\", :values ({:x \"Dietmar_Bauer\", :y 120} {:x \"alanyanews_eu\", :y 55} {:x \"thesirenofangel\", :y 47} {:x \"grimneko\", :y 32} {:x \"2014FL\", :y 30} {:x \"BBattmer\", :y 30} {:x \"S_Tagblatt\", :y 29} {:x \"Klostifan1981\", :y 26} {:x \"ChrisBrey1\", :y 24} {:x \"DietrichOpal\", :y 23} {:x \"martinromanczyk\", :y 22} {:x \"Dennis_Snehotta\", :y 21} {:x \"Liane13\", :y 20} {:x \"andreaslandwehr\", :y 19} {:x \"WOLKEFN\", :y 19})}], :width 1000, :height 618.047, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[(\"Dietmar_Bauer\" \"alanyanews_eu\" \"thesirenofangel\" \"grimneko\" \"2014FL\" \"BBattmer\" \"S_Tagblatt\" \"Klostifan1981\" \"ChrisBrey1\" \"DietrichOpal\" \"martinromanczyk\" \"Dennis_Snehotta\" \"Liane13\" \"andreaslandwehr\" \"WOLKEFN\") #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"1a0f4889-685a-4e77-8947-83dcaa838d4b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"1a0f4889-685a-4e77-8947-83dcaa838d4b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"1a0f4889-685a-4e77-8947-83dcaa838d4b\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"1a0f4889-685a-4e77-8947-83dcaa838d4b\", :values ({:x \"Dietmar_Bauer\", :y 120} {:x \"alanyanews_eu\", :y 55} {:x \"thesirenofangel\", :y 47} {:x \"grimneko\", :y 32} {:x \"2014FL\", :y 30} {:x \"BBattmer\", :y 30} {:x \"S_Tagblatt\", :y 29} {:x \"Klostifan1981\", :y 26} {:x \"ChrisBrey1\", :y 24} {:x \"DietrichOpal\", :y 23} {:x \"martinromanczyk\", :y 22} {:x \"Dennis_Snehotta\", :y 21} {:x \"Liane13\", :y 20} {:x \"andreaslandwehr\", :y 19} {:x \"WOLKEFN\", :y 19})}], :width 1000, :height 618.047, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;; <=

;; **
;;; ### Unique users
;; **

;; @@
(float (/ (->> dpa-users (apply concat) frequencies vals (remove #(> % 1)) count) 
          (->> dpa-users (apply concat) (into #{}) count)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0.6464471</span>","value":"0.6464471"}
;; <=

;; @@

;; @@

;; **
;;; ## August Inspection
;; **

;; @@
(def source-tweet-counts (->> (mc/find-maps @db "users" {:screen_name {$in news-accounts}})
     			    	 (pmap (fn [{:keys [screen_name _id]}] [screen_name (mc/count @db "publications" {:user _id})]))
                         (into {})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/source-tweet-counts</span>","value":"#'unsightly-reserve/source-tweet-counts"}
;; <=

;; @@
(def source-url-counts (->> (mc/find-maps @db "users" {:screen_name {$in news-accounts}})

                         (pmap (fn [{:keys [screen_name _id]}] [screen_name (mc/count @db "urls" {:user _id})]))

                         (into {})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/source-url-counts</span>","value":"#'unsightly-reserve/source-url-counts"}
;; <=

;; @@
(compose
  (bar-chart (keys source-tweet-counts) (vals source-tweet-counts) :plot-size 1000  :colour "red")
  (bar-chart (keys source-url-counts) (vals source-url-counts)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":1000,"height":618.0469970703125,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f","values":[{"x":"dpa","y":520},{"x":"tazgezwitscher","y":700},{"x":"BILD","y":1327},{"x":"ZDFheute","y":949},{"x":"FAZ_NET","y":2919},{"x":"focusonline","y":271},{"x":"welt","y":1990},{"x":"N24_de","y":1962},{"x":"DerWesten","y":947},{"x":"SPIEGELONLINE","y":1763},{"x":"ntvde","y":1816},{"x":"tagesschau","y":1736},{"x":"SZ","y":1615},{"x":"sternde","y":982}]},{"name":"f42733d3-7271-4aa2-a87e-c3412e9b856c","values":[{"x":"dpa","y":445},{"x":"tazgezwitscher","y":639},{"x":"BILD","y":983},{"x":"ZDFheute","y":530},{"x":"FAZ_NET","y":2187},{"x":"focusonline","y":266},{"x":"welt","y":1798},{"x":"N24_de","y":1940},{"x":"DerWesten","y":854},{"x":"SPIEGELONLINE","y":1642},{"x":"ntvde","y":1793},{"x":"tagesschau","y":1282},{"x":"SZ","y":1523},{"x":"sternde","y":895}]}],"marks":[{"type":"rect","from":{"data":"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"red"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}},{"type":"rect","from":{"data":"f42733d3-7271-4aa2-a87e-c3412e9b856c"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 1000, :height 618.047, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f\", :values ({:x \"dpa\", :y 520} {:x \"tazgezwitscher\", :y 700} {:x \"BILD\", :y 1327} {:x \"ZDFheute\", :y 949} {:x \"FAZ_NET\", :y 2919} {:x \"focusonline\", :y 271} {:x \"welt\", :y 1990} {:x \"N24_de\", :y 1962} {:x \"DerWesten\", :y 947} {:x \"SPIEGELONLINE\", :y 1763} {:x \"ntvde\", :y 1816} {:x \"tagesschau\", :y 1736} {:x \"SZ\", :y 1615} {:x \"sternde\", :y 982})} {:name \"f42733d3-7271-4aa2-a87e-c3412e9b856c\", :values ({:x \"dpa\", :y 445} {:x \"tazgezwitscher\", :y 639} {:x \"BILD\", :y 983} {:x \"ZDFheute\", :y 530} {:x \"FAZ_NET\", :y 2187} {:x \"focusonline\", :y 266} {:x \"welt\", :y 1798} {:x \"N24_de\", :y 1940} {:x \"DerWesten\", :y 854} {:x \"SPIEGELONLINE\", :y 1642} {:x \"ntvde\", :y 1793} {:x \"tagesschau\", :y 1282} {:x \"SZ\", :y 1523} {:x \"sternde\", :y 895})}), :marks ({:type \"rect\", :from {:data \"5a7033d4-82fe-4d01-a6ff-6b4f30745d2f\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"red\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}} {:type \"rect\", :from {:data \"f42733d3-7271-4aa2-a87e-c3412e9b856c\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}})}}"}
;; <=

;; **
;;; # Overall Statistics
;; **

;; **
;;; ## Tree Size
;; **

;; @@
(def short-summary 
  (let [users (map :_id (mc/find-maps @db "users" {:screen_name {$in news-accounts}}))
        reaction-forest (pmap #(reaction-tree (:_id %)) (mc/find-maps @db "publications" {:user {$in users}}))]
         (pmap summary reaction-forest)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/short-summary</span>","value":"#'unsightly-reserve/short-summary"}
;; <=

;; @@
(defn short-metrics [coll]
  {:mean (mean coll)
   :variance (variance coll)
   :quantiles (quantile coll)})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/short-metrics</span>","value":"#'unsightly-reserve/short-metrics"}
;; <=

;; @@
(short-metrics (map :size short-summary))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mean</span>","value":":mean"},{"type":"html","content":"<span class='clj-double'>6.257219059342463</span>","value":"6.257219059342463"}],"value":"[:mean 6.257219059342463]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:variance</span>","value":":variance"},{"type":"html","content":"<span class='clj-double'>86.42137086670392</span>","value":"86.42137086670392"}],"value":"[:variance 86.42137086670392]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:quantiles</span>","value":":quantiles"},{"type":"html","content":"<span class='clj-unkown'>(1.0 1.0 3.0 8.0 207.0)</span>","value":"(1.0 1.0 3.0 8.0 207.0)"}],"value":"[:quantiles (1.0 1.0 3.0 8.0 207.0)]"}],"value":"{:mean 6.257219059342463, :variance 86.42137086670392, :quantiles (1.0 1.0 3.0 8.0 207.0)}"}
;; <=

;; @@
(short-metrics (map :height short-summary))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mean</span>","value":":mean"},{"type":"html","content":"<span class='clj-double'>0.9200389803559522</span>","value":"0.9200389803559522"}],"value":"[:mean 0.9200389803559522]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:variance</span>","value":":variance"},{"type":"html","content":"<span class='clj-double'>1.499094212678211</span>","value":"1.499094212678211"}],"value":"[:variance 1.499094212678211]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:quantiles</span>","value":":quantiles"},{"type":"html","content":"<span class='clj-unkown'>(0.0 0.0 1.0 1.0 48.0)</span>","value":"(0.0 0.0 1.0 1.0 48.0)"}],"value":"[:quantiles (0.0 0.0 1.0 1.0 48.0)]"}],"value":"{:mean 0.9200389803559522, :variance 1.499094212678211, :quantiles (0.0 0.0 1.0 1.0 48.0)}"}
;; <=

;; **
;;; ## User Posts
;; **

;; @@
(def user-post-frequencies
  (->> (mc/find-maps @db "publications")
       (map :user)
       frequencies))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/user-post-frequencies</span>","value":"#'unsightly-reserve/user-post-frequencies"}
;; <=

;; @@
(short-metrics (vals user-post-frequencies))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mean</span>","value":":mean"},{"type":"html","content":"<span class='clj-double'>4.354942375886525</span>","value":"4.354942375886525"}],"value":"[:mean 4.354942375886525]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:variance</span>","value":":variance"},{"type":"html","content":"<span class='clj-double'>915.8972380673573</span>","value":"915.8972380673573"}],"value":"[:variance 915.8972380673573]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:quantiles</span>","value":":quantiles"},{"type":"html","content":"<span class='clj-unkown'>(1.0 1.0 1.0 2.0 2919.0)</span>","value":"(1.0 1.0 1.0 2.0 2919.0)"}],"value":"[:quantiles (1.0 1.0 1.0 2.0 2919.0)]"}],"value":"{:mean 4.354942375886525, :variance 915.8972380673573, :quantiles (1.0 1.0 1.0 2.0 2919.0)}"}
;; <=

;; **
;;; ## User Mentions
;; **

;; @@
(def user-mention-frequencies
  (->> (mc/find-maps @db "mentions")
       (map :user)
       frequencies))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/user-mention-frequencies</span>","value":"#'unsightly-reserve/user-mention-frequencies"}
;; <=

;; @@
(short-metrics (vals user-mention-frequencies))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mean</span>","value":":mean"},{"type":"html","content":"<span class='clj-double'>35.81539447190188</span>","value":"35.81539447190188"}],"value":"[:mean 35.81539447190188]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:variance</span>","value":":variance"},{"type":"html","content":"<span class='clj-double'>705749.0909520709</span>","value":"705749.0909520709"}],"value":"[:variance 705749.0909520709]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:quantiles</span>","value":":quantiles"},{"type":"html","content":"<span class='clj-unkown'>(1.0 1.0 2.0 6.0 52035.0)</span>","value":"(1.0 1.0 2.0 6.0 52035.0)"}],"value":"[:quantiles (1.0 1.0 2.0 6.0 52035.0)]"}],"value":"{:mean 35.81539447190188, :variance 705749.0909520709, :quantiles (1.0 1.0 2.0 6.0 52035.0)}"}
;; <=

;; **
;;; ## Hashtags
;; **

;; @@
(def hashtag-frequencies
  (->> (mc/find-maps @db "publications")
       (map :hashtags)
       (remove nil?)
       flatten
       frequencies))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;unsightly-reserve/hashtag-frequencies</span>","value":"#'unsightly-reserve/hashtag-frequencies"}
;; <=

;; @@
(short-metrics (vals hashtag-frequencies))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mean</span>","value":":mean"},{"type":"html","content":"<span class='clj-double'>10.725150555659031</span>","value":"10.725150555659031"}],"value":"[:mean 10.725150555659031]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:variance</span>","value":":variance"},{"type":"html","content":"<span class='clj-double'>10728.604261859029</span>","value":"10728.604261859029"}],"value":"[:variance 10728.604261859029]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:quantiles</span>","value":":quantiles"},{"type":"html","content":"<span class='clj-unkown'>(1.0 1.0 2.0 4.0 6491.0)</span>","value":"(1.0 1.0 2.0 4.0 6491.0)"}],"value":"[:quantiles (1.0 1.0 2.0 4.0 6491.0)]"}],"value":"{:mean 10.725150555659031, :variance 10728.604261859029, :quantiles (1.0 1.0 2.0 4.0 6491.0)}"}
;; <=

;; @@

;; @@
