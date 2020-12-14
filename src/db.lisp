
(in-package zen-db)

(defclass user (store:store-object)
  ((name :accessor name :initarg :name
         :index-type indices:string-unique-index
         :index-reader user-by-name
         :index-keys all-user-names
         :index-values all-users)
   (password :accessor password :initarg :password)
   (groups :accessor groups :initform nil))
  (:metaclass store:persistent-class))

(defclass group (store:store-object)
  ((name :accessor name :initarg :name
         :index-type indices:string-unique-index
         :index-reader group-by-name
         :index-keys all-group-names
         :index-values all-groups))
  (:metaclass store:persistent-class))

(defclass dom (store:store-object)
  ((data :accessor data :initform nil :initarg :data)
   (ns :accessor ns :initform :doms :initarg :class
       :index-type indices:hash-index
       :index-reader ns-doms
       :index-keys all-ns)
   (owner :accessor owner :initarg :owner)
   (group :accessor group :initarg :group)
   (permissions :accessor permissions :initform 640 :initarg permissions))
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
