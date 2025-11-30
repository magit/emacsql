;;; emacsql-pg.el --- EmacSQL back-end for PostgreSQL via pg  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Jonas Bernoulli <emacs.emacsql@jonas.bernoulli.dev>

;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library provides an EmacSQL back-end for PostgreSQL, which
;; uses the `pg' package to directly speak to the database.  This
;; library requires at least Emacs 28.1.

;; (For an alternative back-end for PostgreSQL, see `emacsql-psql'.)

;;; Code:

(require 'emacsql)

(if (>= emacs-major-version 28)
    (require 'pg nil t)
  (message "emacsql-pg.el requires Emacs 28.1 or later"))
(declare-function pg-connect "ext:pg"
                  ( dbname user &optional
                    (password "") (host "localhost") (port 5432) (tls nil)))
(declare-function pg-disconnect "ext:pg" (con))
(declare-function pg-exec "ext:pg" (connection &rest args))
(declare-function pg-result "ext:pg" (result what &rest arg))

(defclass emacsql-pg-connection (emacsql-connection)
  ((pgcon :reader emacsql-pg-pgcon :initarg :pgcon)
   (dbname :reader emacsql-pg-dbname :initarg :dbname)
   (result :accessor emacsql-pg-result)
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "BIGINT")
                      (float "DOUBLE PRECISION")
                      (object "TEXT")
                      (nil "TEXT"))))
  "A connection to a PostgreSQL database via pg.el.")

(cl-defun emacsql-pg (dbname user &key
                             (host "localhost") (password "") (port 5432) debug)
  "Connect to a PostgreSQL server using pg.el."
  (require 'pg)
  (let* ((pgcon (pg-connect dbname user password host port))
         (connection (make-instance 'emacsql-pg-connection
                                    :handle (and (fboundp 'pgcon-process)
                                                 (pgcon-process pgcon))
                                    :pgcon pgcon
                                    :dbname dbname)))
    (when debug (emacsql-enable-debugging connection))
    (emacsql connection [:set (= default-transaction-isolation 'SERIALIZABLE)])
    (emacsql-register connection)))

(cl-defmethod emacsql-close ((connection emacsql-pg-connection))
  (ignore-errors (pg-disconnect (emacsql-pg-pgcon connection))))

(cl-defmethod emacsql-send-message ((connection emacsql-pg-connection) message)
  (condition-case error
      (setf (emacsql-pg-result connection)
            (pg-exec (emacsql-pg-pgcon connection) message))
    (error (signal 'emacsql-error error))))

(cl-defmethod emacsql-waiting-p ((_connection emacsql-pg-connection))
  ;; pg-exec will block
  t)

(cl-defmethod emacsql-parse ((connection emacsql-pg-connection))
  (let ((tuples (pg-result (emacsql-pg-result connection) :tuples)))
    (cl-loop for tuple in tuples collect
             (cl-loop for value in tuple
                      when (stringp value) collect (read value)
                      else collect value))))

(provide 'emacsql-pg)

;;; emacsql-pg.el ends here
