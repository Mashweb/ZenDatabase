
(in-package zen-db)

(defmacro with-db-obj ((obj id) &body body)
  `(let ((,obj (store:store-object-with-id id)))
     (cond (,obj ,@body)
           (t (setf (hunchentoot:return-code*) 404)
              ""))))

(defvar *current-user* nil)
(defvar *auths* (make-hash-table :test #'equal))

(defun proc-auth ()
  (let ((auth (hunchentoot:header-in* "X-Zen-Auth")))
    (when auth
      (destructuring-bind (username token)
          (split-sequence:split-sequence #\: auth)
        (if (equal (gethash token *auths*) username)
            (or (user-by-name username) (http-return 400))
            (http-return 401))))))

(defun include-cors (&optional preflight-p)
  "Include CORS headers. PREFLIGHT-P should be non-nil if it's an OPTION preflight request."
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (when preflight-p
    (setf (hunchentoot:header-out "Access-Control-Allow-Methods") "POST,GET,DELETE,PUT")
    (setf (hunchentoot:header-out "Access-Control-Max-Age") 1000)
    (setf (hunchentoot:header-out "Access-Control-Allow-Headers") "x-requested-with, Content-Encoding, Content-Type, origin, accept")))

(defun check-permissions (object access)
  (let ((access (case access
                  (:r 4)
                  (:w 2)
                  (:x 1))))
    (unless *current-user*
      (http-return 401))
    (or
     ; Owner has access?
     (and (eq *current-user* (owner object))
          (logand access (mod (permissions object) 100)))
     ; Group has access?
     (let ((groups (groups *current-user*)))
       (and (member (group object) groups)
            (logand access (truncate (mod (permissions object) 10) 10))))
     ; World has access?
     (logand access (truncate (permissions object) 100))
     ; Nope
     (http-return 403))))

(defmacro define-operation (api op (&rest args) &body body)
  `(rest:implement-resource-operation ,api ,op ,args
     (catch 'http-return
       (let ((*current-user* (proc-auth)))
         (include-cors)
         ,@body))))

(defmacro http-return (code &optional (reply ""))
  `(progn
     (setf (hunchentoot:return-code*) ,code)
     (throw 'http-return ,reply)))

(defmacro post-args ((&rest args) posted-args &body body)
  (alexandria:with-gensyms (data)
    `(let ((,data (read-from-string ,posted-args)))
       (let ,(loop :for arg :in args
                   :collect (if (listp arg)
                                `(,(first arg) (getf ,data ,(second arg)))
                                `(,arg (getf ,data ,arg))))
         ,@body))))

(rest:define-api zen-db ()
    (:title "Zen Database")
  (user (:produces (:lisp)
         :consumes (:lisp)
         :documentation "Users"
         :path "/users")
        (register (:request-method :post
                   :produces (:lisp)
                   :consumes (:lisp)
                   :path "/users"
                   :documentation "Register a new user")
                  ())
        (get-groups (:request-method :get
                     :produces (:lisp)
                     :path "/users/{id}/groups"
                     :documentation "Get user groups")
                    ((id :integer "User ID")))
        (put-groups (:request-method :put
                     :consumes (:lisp)
                     :path "/users/{id}/groups"
                     :documentation "Put user groups")
                    ((id :integer "User ID")))
        (get-auth-token (:request-method :get
                         :produces (:lisp)
                         :path "/users/{id}/token"
                         :documentation "Get auth token. Send it in a header as `X-Zen-Auth: Username:Token`")
                        ((id :integer "User ID")
                         &optional
                         (pass :string nil "Password"))))
  (dom (:produces (:lisp)
        :consumes (:lisp)
        :documentation "DOM resource"
        :path "/doms")
       (get-doms (:request-method :get
                  :produces (:lisp)
                  :path "/doms"
                  :documentation "Retrieve all doms")
                 ())
       (get-dom (:request-method :get
                 :produces (:lisp)
                 :path "/doms/{id}"
                 :documentation "Retrieve dom")
                ((id :integer "The dom ID")))
       (delete-dom (:request-method :delete
                    :path "/doms/{id}"
                    :documentation "Delete a dom")
                   ((id :integer "The dom ID")))
       (put-dom (:request-method :post
                 :path "/doms"
                 :documentation "Create dom")
                ())
       (chmod (:request-method :put
               :consumes (:lisp)
               :path "/doms/{id}/permissions"
               :documentation "Change permissions. (permissions 640)")
              ((id :integer "The dom ID")))
       (chown (:request-method :put
               :consumes (:lisp)
               :path "/doms/{id}/owner"
               :documentation "Change owner. (owner \"username\" group \"group name\")")
              ((id :integer "The dom ID")))
       ))

;;;; USERS

(define-operation zen-db register (&posted-content posted-content)
  (post-args (name password) posted-content
    (store:with-transaction ()
      (make-instance 'user :name name :password password))))

(define-operation zen-db get-groups (id)
  (with-db-obj (obj id)
    (groups obj)))

(define-operation zen-db put-groups (id &posted-content posted-content)
  (post-args (groups) posted-content
    (with-transaction ()
      (dolist (group-name groups)
        (let ((group (group-by-name group-name)))
          (unless group
            (make-instance 'group :name group-name))))
      (with-db-obj (obj id)
        (setf (groups obj) groups)))))

(define-operation zen-db get-auth-token (id &key pass)
  (with-db-obj (obj id)
    (if (string= pass (password obj))
        (let ((token (random-string)))
          (setf (gethash id *auths*) token)
          token)
        (http-return 403 "Invalid password"))))

;;;; DOMS

(define-operation zen-db get-doms ()
  (mapcar #'data (store:store-objects-with-class 'dom)))

(define-operation zen-db get-dom (id)
  (with-db-obj (obj id)
    (check-permissions obj :r)
    (data obj)))

(define-operation zen-db delete-dom (id)
  (with-db-obj (obj id)
    (check-permissions obj :w)
    (store:delete-object obj)
    (http-return 204))) ;; No Content

(define-operation zen-db put-dom (&posted-content posted-content)
  (unless *current-user*
    (http-return 401))
  (store:with-transaction ()
    (let ((obj
            (make-instance 'dom
                           :data (read-from-string (babel:octets-to-string
                                                    posted-content))
                           :owner *current-user*
                           :group (group-by-name (name *current-user*)))))
      (setf (hunchentoot:header-out :location)
            (format nil "/doms/~A" (store:store-object-id obj)))
      (http-return 201))))

(define-operation zen-db chmod (id &posted-content posted-content)
  (post-args (permissions) posted-content
    (with-db-obj (obj id)
      (check-permissions obj :w)
      (setf (permissions obj) permissions))))

(define-operation zen-db chown (id &posted-content posted-content)
  (post-args (owner group) posted-content
    (with-db-obj (obj id)
      (check-permissions obj :w)
      (unless (or new-owner new-group)
        (http-return 400))
      (when new-owner
        (setf (owner obj) (user-by-name owner)))
      (when new-group
        (setf (group obj) (group-by-name group))))))

;;;; MISC

(defun start-server ()
  (with-config zen-db (http-address http-port)
    (rest:start-api 'zen-db :address http-address :port http-port)))
