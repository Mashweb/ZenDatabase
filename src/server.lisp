
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
       (delete-dom (:request-method :delete
                                    :path "/doms/{id}"
                                    :documentation "Delete a dom")
                ((id :integer "The dom ID")))
       (put-dom (:request-method :post
                                 :path "/doms"
                                 :documentation "Create dom")
                ())))

(defun include-cors (&optional preflight-p)
  "Include CORS headers. PREFLIGHT-P should be non-nil if it's an OPTION preflight request."
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (when preflight-p
    (setf (hunchentoot:header-out "Access-Control-Allow-Methods") "POST,GET,DELETE,PUT")
    (setf (hunchentoot:header-out "Access-Control-Max-Age") 1000)
    (setf (hunchentoot:header-out "Access-Control-Allow-Headers") "x-requested-with, Content-Encoding, Content-Type, origin, accept")))

(rest:implement-resource-operation zen-db get-doms ()
  (include-cors)
  (mapcar #'data (store:store-objects-with-class 'dom)))

(rest:implement-resource-operation zen-db get-dom (id)
  (include-cors)
  (let ((obj (store:store-object-with-id id)))
    (cond (obj (data obj))
          (t (setf (hunchentoot:return-code*) 404)
             ""))))

(rest:implement-resource-operation zen-db delete-dom (id)
  (include-cors)
  (store:delete-object (store:store-object-with-id id))
  (setf (hunchentoot:return-code*) 204) ;; No Content
  "")

(rest:implement-resource-operation zen-db put-dom (&posted-content posted-content)
  (include-cors)
  (store:with-transaction ()
    (let ((obj
           (make-instance 'dom :data
                          (read-from-string (babel:octets-to-string
                                             posted-content)))))
      (setf (hunchentoot:header-out :location)
            (format nil "/doms/~A" (store:store-object-id obj)))
      (setf (hunchentoot:return-code*) 201) ;; Created
      "")))

(defun start-server ()
  (with-config zen-db (http-address http-port)
    (rest:start-api 'zen-db :address http-address :port http-port)))
