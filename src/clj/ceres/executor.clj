(ns ceres.executor
  (:require [clojurewerkz.quartzite.scheduler :as qs]
            [clojurewerkz.quartzite.triggers :as t]
            [clojurewerkz.quartzite.jobs :as j]
            [clojurewerkz.quartzite.conversion :as qc]
            [ceres.curator :as curator]
            [taoensso.timbre :as timbre])
  (:use [clojurewerkz.quartzite.jobs :only [defjob]]
        [clojurewerkz.quartzite.schedule.daily-interval :only [schedule with-repeat-count with-interval-in-days with-interval-in-minutes time-of-day on-every-day]]))

(timbre/refer-timbre)

(defjob TweetBackup
  [ctx]
  (let [path (get (qc/from-job-data ctx) "folder-path")]
      (info "Writing tweets backup...")
    (curator/backup-tweets path)))


(defn start-executor [path]
  (swap! executor-state #(assoc %1 :backup-folder %2) path)
  (qs/initialize)
  (qs/start)
  (let [job (j/build
             (j/of-type TweetBackup)
             (j/using-job-data {"folder-path" path})
             (j/with-identity (j/key "jobs.tweetscount.1")))
        tk (t/key "triggers.1")
        trigger (t/build
                 (t/with-identity tk)
                 (t/start-now)
                 (t/with-schedule
                   (schedule
                    (starting-daily-at (time-of-day 2 00 00)))))]
    (qs/schedule job trigger)))


(comment
  (qs/initialize)

  (qs/start)

  (start-executor "/home/konny/data")

  (let [job (j/build
              (j/of-type TweetsCount)
              (j/with-identity (j/key "jobs.tweetscount.1")))
        trigger (t/build
                  (t/with-identity (t/key "triggers.1"))
                  (t/start-now)
                  (t/with-schedule (schedule
                                     (with-repeat-count 60)
                                     (with-interval-in-seconds 10))))]
    (qs/schedule job trigger))

  (qs/shutdown)

  (schedule (with-interval-in-days 1) (time-of-day 2 00 00))


  (every-day)

  (time-of-day 16 00 00)

  )
