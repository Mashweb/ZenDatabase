
(defsystem zen-db
  :author "Alexander Sukhoverkhov"
  :license "Apache License 2.0"
  :depends-on (alexandria ; general
               crane ; db
               rest-server ; web
               )
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "db")
               (:file "server")
               )
  )
