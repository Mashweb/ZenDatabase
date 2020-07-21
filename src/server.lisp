
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
  (data (store:store-object-with-id id)))

(rest:implement-resource-operation zen-db put-dom (&posted-content posted-content)
  (include-cors)
  (store:with-transaction ()
    (let ((obj
           (make-instance 'dom :data
                          (read-from-string (babel:octets-to-string
                                             posted-content)))))
      (setf (hunchentoot:header-out :location)
            (format nil "/doms/~A" (store:store-object-id obj)))
      (setf (hunchentoot:return-code*) 201)
      "")))

(defun start-server ()
  (with-config zen-db (http-address http-port)
    (rest:start-api 'zen-db :address http-address :port http-port)))
