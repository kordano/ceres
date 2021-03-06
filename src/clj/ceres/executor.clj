(ns ceres.executor
  (:require [clojurewerkz.quartzite.scheduler :as qs]
            [clojurewerkz.quartzite.triggers :as t]
            [clojurewerkz.quartzite.jobs :as j]
            [clojurewerkz.quartzite.conversion :as qc]
            [clojurewerkz.quartzite.jobs :refer [defjob]]
            [clojurewerkz.quartzite.schedule.daily-interval :refer [schedule time-of-day every-day starting-daily-at ending-daily-at]]
            [ceres.curator :as curator]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defjob ArticlesBackup [ctx]
  (let [path (get (qc/from-job-data ctx) "folder-path")]
      (info "Writing articles backup...")
    (curator/backup-yesterday "athena" "articles" path)))


(defjob TweetBackup [ctx]
  (let [path (get (qc/from-job-data ctx) "folder-path")]
      (info "Writing tweets backup...")
      (curator/backup-yesterday "athena" "tweets" path)))


(defjob OriginBackup [ctx]
  (let [path (get (qc/from-job-data ctx) "folder-path")]
      (info "Writing origins backup...")
    (curator/backup-yesterday "athena" "origins" path)))


(defn tweets-backup-schedule
  "Create a schedule to backup the tweets at 3 am"
  [path]
  (let [job (j/build
             (j/of-type TweetBackup)
             (j/using-job-data {"folder-path" path})
             (j/with-identity (j/key "jobs.tweetsbackup.1")))
        tk (t/key "triggers.1")
        trigger (t/build
                 (t/with-identity tk)
                 (t/start-now)
                 (t/with-schedule
                   (schedule
                    (every-day)
                    (starting-daily-at (time-of-day 3 00 00))
                    (ending-daily-at (time-of-day 3 00 01)))))]
    (qs/schedule job trigger)))


(defn articles-backup-schedule
  "Create a schedule to backup the articles at 3.05 am"
  [path]
  (let [job (j/build
             (j/of-type ArticlesBackup)
             (j/using-job-data {"folder-path" path})
             (j/with-identity (j/key "jobs.articlesbackup.1")))
        tk (t/key "triggers.2")
        trigger (t/build
                 (t/with-identity tk)
                 (t/start-now)
                 (t/with-schedule
                   (schedule
                    (every-day)
                    (starting-daily-at (time-of-day 3 05 00))
                    (ending-daily-at (time-of-day 3 05 01)))))]
    (qs/schedule job trigger)))


(defn origins-backup-schedule
  "Create a schedule to backup the origins at 3.10 am"
  [path]
  (let [job (j/build
             (j/of-type OriginBackup)
             (j/using-job-data {"folder-path" path})
             (j/with-identity (j/key "jobs.originsbackup.1")))
        tk (t/key "triggers.3")
        trigger (t/build
                 (t/with-identity tk)
                 (t/start-now)
                 (t/with-schedule
                   (schedule
                    (every-day)
                    (starting-daily-at (time-of-day 3 10 00))
                    (ending-daily-at (time-of-day 3 10 01)))))]
    (qs/schedule job trigger)))


(defn start-executor
  "Run the schedules"
  [path]
  (qs/initialize)
  (qs/start)
  (tweets-backup-schedule path)
  (articles-backup-schedule path)
  (origins-backup-schedule path))
