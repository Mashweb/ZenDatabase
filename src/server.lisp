
(in-package zen-db)

(rest:define-api zen-db ()
    (:title "Zen Database")
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
       (put-dom (:request-method :post
                                 :produces ()
                                 :path "/doms"
                                 :documentation "Create dom")
                ())))

(rest:implement-resource-operation zen-db get-doms ()
  (mapcar #'data (store:store-objects-with-class 'dom)))

(rest:implement-resource-operation zen-db get-dom (id)
  (data (store:store-object-with-id id)))

(rest:implement-resource-operation zen-db put-dom (&posted-content posted-content)
  (store:with-transaction ()
    (let ((obj
           (make-instance 'dom :data
                          (read-from-string (babel:octets-to-string
                                             posted-content)))))
      (setf (hunchentoot:header-out :location)
            (format nil "/doms/~A" (store:store-object-id obj)))
      (setf (hunchentoot:return-code*) 201)
      t)))

(defun start-server ()
  (with-config zen-db (http-address http-port)
    (rest:start-api 'zen-db :address http-address :port http-port)))
