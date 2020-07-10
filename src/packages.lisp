
(defpackage zen-db
  (:use :cl)
  (:local-nicknames (:rest :rest-server)
                    (:store :bknr.datastore)
                    (:indices :bknr.indices))
  (:export #:start))
