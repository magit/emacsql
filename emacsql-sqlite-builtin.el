;;; emacsql-sqlite-builtin.el --- EmacSQL back-end for SQLite using builtin support  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacsql
;; Version: 3.0.0-git
;; Package-Requires: ((emacs "29") (emacsql "3.0.0") (emacsql-sqlite "3.0.0"))

;;; Commentary:

;; EmacSQL back-end for SQLite using builtin support added in Emacs 29.

;;; Code:

(require 'sqlite)
(require 'emacsql)
;; For `emacsql-sqlite-reserved' and `emacsql-sqlite-condition-alist'.
(require 'emacsql-sqlite)

(defclass emacsql-sqlite-builtin-connection (emacsql-connection)
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
  (:documentation "A connection to a SQLite database using builtin support."))

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-builtin-connection) &rest _)
  (setf (emacsql-process connection)
        (sqlite-open (slot-value connection 'file)))
  (when emacsql-global-timeout
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2)))
  (emacsql-register connection))

(cl-defun emacsql-sqlite-builtin (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance #'emacsql-sqlite-builtin-connection
                                   :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-live-p ((connection emacsql-sqlite-builtin-connection))
  (and (emacsql-process connection) t))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-builtin-connection))
  (sqlite-close (emacsql-process connection))
  (setf (emacsql-process connection) nil))

(cl-defmethod emacsql-send-message
  ((connection emacsql-sqlite-builtin-connection) message)
  (condition-case err
      (mapcar (lambda (row)
                (mapcar (lambda (col)
                          (cond ((null col) nil)
                                ((equal col "") "")
                                ((numberp col) col)
                                (t (read col))))
                        row))
              (sqlite-select (emacsql-process connection) message nil nil))
    ((db-error sql-error)
     (pcase-let ((`(,sym ,msg ,code) err))
       (signal (or (cadr (cl-assoc code emacsql-sqlite-condition-alist
                                   :test #'memql))
                   'emacsql-error)
               (list msg code sym))))
    (error
     (signal 'emacsql-error err))))

(cl-defmethod emacsql ((connection emacsql-sqlite-builtin-connection) sql &rest args)
  (emacsql-send-message connection (apply #'emacsql-compile connection sql args)))

(provide 'emacsql-sqlite-builtin)

;;; emacsql-sqlite-builtin.el ends here
