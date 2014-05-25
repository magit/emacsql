;;; emacsql-pg.el --- back-end for PostgreSQL via pg -*- lexical-binding: t; -*-

;;; Commentary:

;; Unlike emacsql-psql, this connection type uses Eric Marsden's pg.el
;; to connect to PostgreSQL. It speaks directly to the database, so
;; unlike the other EmacSQL connection types, this one requires no
;; external command line programs.

;; The only pg functions required are pg:connect, pg:disconnect,
;; pg:exec, and pg:result. Unfortunately, since pg.el is synchronous
;; it will not be fully compliant once EmacSQL supports asynchronous
;; queries. But, on the plus side, this means the implementation below
;; is dead simple.

;;; Code:

(require 'pg)
(require 'eieio)
(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-psql)  ; for reserved words

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
  (:documentation "A connection to a PostgreSQL database via pg.el."))

(cl-defun emacsql-pg (dbname user &key
                             (host "localhost") (password "") (port 5432) debug)
  "Connect to a PostgreSQL server using pg.el."
  (let* ((pgcon (pg:connect dbname user password host port))
         (connection (make-instance 'emacsql-pg-connection
                                    :process (pgcon-process pgcon)
                                    :pgcon pgcon
                                    :dbname dbname)))
    (when debug (emacsql-enable-debugging connection))
    (emacsql connection [:set (= default-transaction-isolation 'SERIALIZABLE)])
    (emacsql-register connection)))

(defmethod emacsql-close ((connection emacsql-pg-connection))
  (ignore-errors (pg:disconnect (emacsql-pg-pgcon connection))))

(defmethod emacsql-send-message ((connection emacsql-pg-connection) message)
  (condition-case error
      (setf (emacsql-pg-result connection)
            (pg:exec (emacsql-pg-pgcon connection) message))
    (error (signal 'emacsql-error error))))

(defmethod emacsql-waiting-p ((_connection emacsql-pg-connection))
  ;; pg:exec will block
  t)

(defmethod emacsql-parse ((connection emacsql-pg-connection))
  (let ((tuples (pg:result (emacsql-pg-result connection) :tuples)))
    (cl-loop for tuple in tuples collect
             (cl-loop for value in tuple
                      when (stringp value) collect (read value)
                      else collect value))))

(provide 'emacsql-pg)

;;; emacsql-pg.el ends here
