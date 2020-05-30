
(defsystem zen-db
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
