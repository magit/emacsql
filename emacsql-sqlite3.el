;;; emacsql-sqlite.el --- EmacSQL back-end for SQLite -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; URL: https://github.com/skeeto/emacsql
;; Package-Requires: ((emacs "25.1") (emacsql "2.0.0") (sqlite3-api "0.11"))

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)
(require 'url)
(require 'url-http)
(require 'emacsql)
(require 'sqlite3-api)
(require 'subr-x)

(defclass emacsql-sqlite3-connection (emacsql-connection)
  ((handle :initarg :handle)
   (file :initarg :file
         :type (or null string)
         :documentation "Database file name.")
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "INTEGER")
                      (float "REAL")
                      (object "TEXT")
                      (nil nil)))
  (:documentation
   "A connection to a SQLite database using the `sqlite3-api' module.")))

(cl-defun emacsql-sqlite3 (file &key debug)
  (let ((connection (make-instance 'emacsql-sqlite3-connection :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite3-connection) &rest _)
  (oset connection handle
        (sqlite3-open (if-let ((file (slot-value connection 'file)))
                          (expand-file-name file)
                        ":memory:")
                      sqlite-open-readwrite
                      sqlite-open-create))
  (emacsql-register connection))

(cl-defmethod emacsql-close ((connection emacsql-sqlite3-connection))
  (sqlite3-close (oref connection handle)))

(cl-defmethod emacsql ((connection emacsql-sqlite3-connection) sql &rest args)
  (pcase-let* ((`(,sql . ,params)
                (apply #'emacsql-compile connection sql args))
               (stmt (sqlite3-prepare (oref connection handle) sql))
               (i 1)
               (ret nil))
    (dolist (param params)
      (cl-typecase param
        (null    (sqlite3-bind-null  stmt i))
        (integer (sqlite3-bind-int64 stmt i param))
        (string  (sqlite3-bind-text  stmt i (prin1-to-string param)))
        (t       (sqlite3-bind-text  stmt i (prin1-to-string param))))
      (cl-incf i))
    (while (= (sqlite3-step stmt) sqlite-row)
      (push (mapcar (lambda (s)
                      (if (stringp s)
                          (car (read-from-string s))
                        s))
                    (sqlite3-fetch stmt))
            ret))
    (sqlite3-finalize stmt)
    (nreverse ret)))

(cl-defmethod emacsql-compile ((connection emacsql-sqlite3-connection) sql &rest args)
  (let ((emacsql-type-map (or (and connection (emacsql-types connection))
                              emacsql-type-map)))
    (emacsql-format-partial (emacsql-prepare sql) args)))

(defun emacsql-format-partial (expansion args)
  (cl-destructuring-bind (format . vars) expansion
    (let (params)
      (cons
       (apply #'format format
              (cl-loop for (i . kind) in vars collect
                       (let ((thing (nth i args)))
                         (cl-case kind
                           (:identifier (emacsql-escape-identifier thing))
                           (:scalar     (push thing params) "?")
                           (:vector     (dolist (thing (cl-coerce thing 'list))
                                          (push thing params))
                                        (emacsql-escape-vector-partial thing))
                           (:raw        (emacsql-escape-raw thing))
                           (:schema     (emacsql-prepare-schema thing))
                           (otherwise
                            (emacsql-error "Invalid var type %S" kind))))))
       (nreverse params)))))

(defun emacsql-escape-vector-partial (vector)
  (cl-typecase vector
    (null   (emacsql-error "Empty SQL vector expression."))
    (list   (mapconcat #'emacsql-escape-vector-partial vector ", "))
    (vector (concat "(" (mapconcat (lambda (_) "?") vector ", ")")"))
    (otherwise (emacsql-error "Invalid vector %S" vector))))

(provide 'emacsql-sqlite3)

;;; emacsql-sqlite3.el ends here
