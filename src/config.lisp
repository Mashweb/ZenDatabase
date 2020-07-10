
(in-package zen-db)

(defun find-config (id)
  (let ((id (string-downcase id)))
    (loop :for path :in (uiop:xdg-config-pathnames)
       :for full-path := (format nil "~A~A.conf" path id)
       :if (uiop:probe-file* full-path)
       :do (return full-path))
    (format nil "~A~A.conf" (if (uiop:xdg-config-pathnames)
                                (first (uiop:xdg-config-pathnames))
                                (user-homedir-pathname))
            id)))

(defun load-config (id)
  (let ((pathname (find-config id)))
    (with-open-file (in pathname
                        :direction :input
                        :if-does-not-exist :error)
      (loop :for form := (read in nil 'eof)
         :until (eq form 'eof)
         :append form))))

(defun save-config (id config)
  (let ((pathname (find-config id)))
    (with-open-file (out pathname
                         :direction :output
                         :if-exists :supersede)
      (loop :for (key value) :on config :by #'cddr
         :do (format out "(~S ~S)~%" key value)))))

(defun ensure-config (id &rest clauses)
  (let ((config (ignore-errors (load-config id))))
    (dolist (clause clauses)
      (destructuring-bind (key value)
          clause
        (when (eq 'undefined (getf config key 'undefined))
          (setf (getf config key) value))))
    (save-config id config)))

(defmacro with-config (id vars &body body)
  (alexandria:with-gensyms (config)
    `(let ((,config (load-config ',id)))
       (let ,(loop :for var :in vars
                :collect `(,var (getf ,config ',var)))
         ,@body))))

