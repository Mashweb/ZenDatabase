
(in-package zen-db)

(defclass dom (store:store-object)
  ((data :accessor data :initform nil :initarg :data)
   (ns :accessor ns :initform :doms :initarg :class
       :index-type indices:hash-index
       :index-reader ns-doms
       :index-keys all-ns))
  (:metaclass store:persistent-class))

(defun start-db ()
  (with-config zen-db (db-dir)
    (make-instance 'store:mp-store
                   :directory db-dir
                   :subsystems (list
                                (make-instance
                                 'store:store-object-subsystem))))
  (cl-cron:make-cron-job 'snapshot :minute 0 :hour 0)
  (cl-cron:start-cron))

(defun snapshot ()
  (store:snapshot))
