
(in-package zen-db)

(defun init-conf ()
  (ensure-config 'zen-db
                 `(db-dir ,(uiop:xdg-data-home "zen-db/"))
                 '(http-address "localhost")
                 '(http-port 8080))
  (format t "Using config file: ~A~%" (find-config 'zen-db)))

(defun start ()
  (init-conf)
  (start-db)
  (start-server))
