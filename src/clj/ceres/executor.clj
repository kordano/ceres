(ns ceres.executor
  (:require [clojurewerkz.quartzite.scheduler :as qs]
            [clojurewerkz.quartzite.triggers :as t]
            [clojurewerkz.quartzite.jobs :as j]
            [clojurewerkz.quartzite.conversion :as qc]
            [ceres.curator :as curator]
            [taoensso.timbre :as timbre])
  (:use [clojurewerkz.quartzite.jobs :only [defjob]]
        [clojurewerkz.quartzite.schedule.daily-interval :only [schedule with-repeat-count with-interval-in-days with-interval-in-minutes time-of-day every-day starting-daily-at ending-daily-at]]))

(timbre/refer-timbre)


(defjob ArticlesBackup
  [ctx]
  (let [path (get (qc/from-job-data ctx) "folder-path")]
      (info "Writing articles backup...")
    (curator/backup-articles path)))


(defjob TweetBackup
  [ctx]
  (let [path (get (qc/from-job-data ctx) "folder-path")]
      (info "Writing tweets backup...")
    (curator/backup-tweets path)))


(defn tweets-backup-schedule [path]
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


(defn articles-backup-schedule [path]
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


(defn start-executor [path]
  (qs/initialize)
  (qs/start)
  (tweets-backup-schedule)
  (articles-backup-schedule))
