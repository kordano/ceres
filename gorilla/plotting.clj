;; gorilla-repl.fileformat = 1

;; **
;;; # Ceres Analysis
;; **

;; @@
(require '[gorilla-plot.core :refer :all])
(require '[ceres.collector :refer :all])
(require '[ceres.curator :refer [start-date end-date]])
(require '[monger.collection :as mc])
(require '[clj-time.core :as t])
(require '[clj-time.periodic :as p])
(require '[monger.operators :refer :all])
(require '[monger.query :refer :all])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Average Tweets per Day
;; **

;; @@
(let [days-running (t/in-days (t/interval start-date end-date))
        dates (take days-running (p/periodic-seq start-date (t/days 1)))
        tweet-count (map #(mc/count @db "tweets" {:created_at {$gte % $lt (t/plus % (t/days 1))}}) dates)
        days (range  2 (inc (inc days-running)))]
    (list-plot tweet-count :joined true))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"250752ca-3ec3-4885-9492-98382dda6430","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"250752ca-3ec3-4885-9492-98382dda6430","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"250752ca-3ec3-4885-9492-98382dda6430"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"250752ca-3ec3-4885-9492-98382dda6430","values":[{"x":0,"y":626},{"x":1,"y":12959},{"x":2,"y":12098},{"x":3,"y":15569},{"x":4,"y":14210},{"x":5,"y":9976},{"x":6,"y":10310},{"x":7,"y":21440},{"x":8,"y":15433},{"x":9,"y":11617},{"x":10,"y":11313},{"x":11,"y":10274},{"x":12,"y":20489},{"x":13,"y":12970},{"x":14,"y":13832},{"x":15,"y":11037},{"x":16,"y":11973},{"x":17,"y":10438},{"x":18,"y":7933},{"x":19,"y":8798},{"x":20,"y":10314},{"x":21,"y":11652},{"x":22,"y":10922},{"x":23,"y":12110},{"x":24,"y":12524},{"x":25,"y":8184},{"x":26,"y":9900},{"x":27,"y":10283},{"x":28,"y":10477},{"x":29,"y":10285},{"x":30,"y":9479},{"x":31,"y":8774},{"x":32,"y":6783},{"x":33,"y":9144},{"x":34,"y":9718},{"x":35,"y":9909},{"x":36,"y":11237},{"x":37,"y":10819},{"x":38,"y":9143},{"x":39,"y":6782},{"x":40,"y":8274},{"x":41,"y":7935},{"x":42,"y":12168},{"x":43,"y":11173},{"x":44,"y":10445},{"x":45,"y":10248},{"x":46,"y":7808},{"x":47,"y":7729},{"x":48,"y":8024},{"x":49,"y":12345},{"x":50,"y":9617},{"x":51,"y":8439},{"x":52,"y":8807},{"x":53,"y":7077},{"x":54,"y":7140},{"x":55,"y":8550},{"x":56,"y":9374},{"x":57,"y":9372},{"x":58,"y":11006},{"x":59,"y":9324},{"x":60,"y":7472},{"x":61,"y":10782},{"x":62,"y":11107},{"x":63,"y":10597},{"x":64,"y":10699},{"x":65,"y":117},{"x":66,"y":5755},{"x":67,"y":0},{"x":68,"y":0},{"x":69,"y":7386},{"x":70,"y":10509},{"x":71,"y":9096},{"x":72,"y":10668},{"x":73,"y":11045},{"x":74,"y":7897},{"x":75,"y":7284},{"x":76,"y":0},{"x":77,"y":3966},{"x":78,"y":8720},{"x":79,"y":10049},{"x":80,"y":9311},{"x":81,"y":8080},{"x":82,"y":8251},{"x":83,"y":9994},{"x":84,"y":11015},{"x":85,"y":10832},{"x":86,"y":10015},{"x":87,"y":10144},{"x":88,"y":7677},{"x":89,"y":8751},{"x":90,"y":9907},{"x":91,"y":10538}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"250752ca-3ec3-4885-9492-98382dda6430\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"250752ca-3ec3-4885-9492-98382dda6430\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"250752ca-3ec3-4885-9492-98382dda6430\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"250752ca-3ec3-4885-9492-98382dda6430\", :values ({:x 0, :y 626} {:x 1, :y 12959} {:x 2, :y 12098} {:x 3, :y 15569} {:x 4, :y 14210} {:x 5, :y 9976} {:x 6, :y 10310} {:x 7, :y 21440} {:x 8, :y 15433} {:x 9, :y 11617} {:x 10, :y 11313} {:x 11, :y 10274} {:x 12, :y 20489} {:x 13, :y 12970} {:x 14, :y 13832} {:x 15, :y 11037} {:x 16, :y 11973} {:x 17, :y 10438} {:x 18, :y 7933} {:x 19, :y 8798} {:x 20, :y 10314} {:x 21, :y 11652} {:x 22, :y 10922} {:x 23, :y 12110} {:x 24, :y 12524} {:x 25, :y 8184} {:x 26, :y 9900} {:x 27, :y 10283} {:x 28, :y 10477} {:x 29, :y 10285} {:x 30, :y 9479} {:x 31, :y 8774} {:x 32, :y 6783} {:x 33, :y 9144} {:x 34, :y 9718} {:x 35, :y 9909} {:x 36, :y 11237} {:x 37, :y 10819} {:x 38, :y 9143} {:x 39, :y 6782} {:x 40, :y 8274} {:x 41, :y 7935} {:x 42, :y 12168} {:x 43, :y 11173} {:x 44, :y 10445} {:x 45, :y 10248} {:x 46, :y 7808} {:x 47, :y 7729} {:x 48, :y 8024} {:x 49, :y 12345} {:x 50, :y 9617} {:x 51, :y 8439} {:x 52, :y 8807} {:x 53, :y 7077} {:x 54, :y 7140} {:x 55, :y 8550} {:x 56, :y 9374} {:x 57, :y 9372} {:x 58, :y 11006} {:x 59, :y 9324} {:x 60, :y 7472} {:x 61, :y 10782} {:x 62, :y 11107} {:x 63, :y 10597} {:x 64, :y 10699} {:x 65, :y 117} {:x 66, :y 5755} {:x 67, :y 0} {:x 68, :y 0} {:x 69, :y 7386} {:x 70, :y 10509} {:x 71, :y 9096} {:x 72, :y 10668} {:x 73, :y 11045} {:x 74, :y 7897} {:x 75, :y 7284} {:x 76, :y 0} {:x 77, :y 3966} {:x 78, :y 8720} {:x 79, :y 10049} {:x 80, :y 9311} {:x 81, :y 8080} {:x 82, :y 8251} {:x 83, :y 9994} {:x 84, :y 11015} {:x 85, :y 10832} {:x 86, :y 10015} {:x 87, :y 10144} {:x 88, :y 7677} {:x 89, :y 8751} {:x 90, :y 9907} {:x 91, :y 10538})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=
