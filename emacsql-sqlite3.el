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
  (let (ret)
    (sqlite3-exec (oref connection handle)
                  (apply #'emacsql-compile connection sql args)
                  (lambda (_ncols data _names)
                    (push (mapcar (lambda (s)
                                    (if (stringp s)
                                        (car (read-from-string s))
                                      s))
                                  data)
                          ret)))
    (nreverse ret)))

(provide 'emacsql-sqlite3)

;;; emacsql-sqlite3.el ends here
