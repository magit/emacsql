;;; emacsql-sqlite-module.el --- EmacSQL back-end for SQLite using a module  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacsql
;; Version: 3.0.0-git
;; Package-Requires: ((emacs "29") (emacsql "3.0.0") (emacsql-sqlite "3.0.0"))

;;; Commentary:

;; EmacSQL back-end for SQLite using a module provided by the `sqlite3'
;; package from https://github.com/pekingduck/emacs-sqlite3-api.

;;; Code:

(require 'sqlite3)
(require 'emacsql)
;; For `emacsql-sqlite-reserved' and `emacsql-sqlite-condition-alist'.
(require 'emacsql-sqlite)

(defclass emacsql-sqlite-module-connection (emacsql-connection)
  ((file :initarg :file
         :type (or null string)
         :documentation "Database file name.")
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "INTEGER")
                      (float "REAL")
                      (object "TEXT")
                      (nil nil)))
   ;; Cannot use `process' slot because we cannot completely
   ;; change the type of a slot, just make it more specific.
   (handle :documentation "Database handle."
           :accessor emacsql-process))
  (:documentation "A connection to a SQLite database using a module."))

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-module-connection) &rest _)
  (setf (emacsql-process connection)
        (sqlite3-open (or (slot-value connection 'file) ":memory:")
                      sqlite-open-readwrite
                      sqlite-open-create))
  (when emacsql-global-timeout
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2)))
  (emacsql-register connection))

(cl-defun emacsql-sqlite-module (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance #'emacsql-sqlite-module-connection
                                   :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-live-p ((connection emacsql-sqlite-module-connection))
  (and (emacsql-process connection) t))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-module-connection))
  (sqlite3-close (emacsql-process connection))
  (setf (emacsql-process connection) nil))

(cl-defmethod emacsql-send-message
  ((connection emacsql-sqlite-module-connection) message)
  (let (rows)
    (condition-case err
        (sqlite3-exec (emacsql-process connection)
                      message
                      (lambda (_ row _)
                        (push (mapcar (lambda (col)
                                        (cond ((null col) nil)
                                              ((equal col "") "")
                                              (t (read col))))
                                      row)
                              rows)))
      ((db-error sql-error)
       (pcase-let ((`(,sym ,msg ,code) err))
         (signal (or (cadr (cl-assoc code emacsql-sqlite-condition-alist
                                     :test #'memql))
                     'emacsql-error)
                 (list msg code sym))))
      (error
       (signal 'emacsql-error err)))
    (nreverse rows)))

(cl-defmethod emacsql ((connection emacsql-sqlite-module-connection) sql &rest args)
  (emacsql-send-message connection (apply #'emacsql-compile connection sql args)))

(provide 'emacsql-sqlite-module)

;;; emacsql-sqlite-module.el ends here
