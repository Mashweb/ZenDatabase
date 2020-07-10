
(defsystem zen-db
  :author "Alexander Sukhoverkhov"
  :license "Apache License 2.0"
  :depends-on (alexandria cl-cron babel ; general
               bknr.datastore ; db
               hunchentoot rest-server ; web
               )
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "db")
               (:file "server")
               (:file "main")
               ))
