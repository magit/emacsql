;;; emacsql-psql.el --- PostgreSQL front-end for Emacsql -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'emacsql)

(defvar emacsql-psql-executable "psql"
  "Path to the psql (PostgreSQL client) executable.")

(defun emacsql-psql-unavailable-p ()
  "Return a reason if the psql executable is not available.
:no-executable -- cannot find the executable
:cannot-execute -- cannot run the executable
:old-version -- sqlite3 version is too old"
  (let ((psql emacsql-psql-executable))
    (if (null (executable-find psql))
        :no-executable
      (condition-case _
          (with-temp-buffer
            (call-process psql nil (current-buffer) nil "--version")
            (let ((version (cl-third (split-string (buffer-string)))))
              (if (version< version "1.0.0")
                  :old-version
                nil)))
        (error :cannot-execute)))))

(defclass emacsql-psql-connection (emacsql-connection)
  ((dbname :reader emacsql-psql-dbname :initarg :dbname))
  (:documentation "A connection to a PostgreSQL database."))

(cl-defun emacsql-psql (dbname &key username hostname port)
  "Connect to a PostgreSQL server using the psql command line program."
  (let ((args (list dbname)))
    (when username
      (push username args))
    (push "-n" args)
    (when port
      (push "-p" args)
      (push port args))
    (when hostname
      (push "-h" args)
      (push hostname args))
    (setf args (nreverse args))
    (let* ((buffer (generate-new-buffer "*emacsql-psql*"))
           (psql emacsql-psql-executable)
           (process (apply #'start-process "emacsql-psql" buffer psql args))
           (connection (make-instance 'emacsql-psql-connection
                                      :process process
                                      :dbname dbname)))
      (prog1 connection
        (emacsql-register connection)
        (emacsql-send-string connection "\\pset pager off")
        (emacsql-send-string connection "\\a")
        (emacsql-send-string connection "\\t")
        (emacsql-send-string connection "\\f ' '")
        (emacsql-send-string connection "\\set PROMPT1 ]")
        (emacsql-send-string connection "EMACSQL;") ; error message flush
        (emacsql-wait connection)))))

(defmethod emacsql-close ((connection emacsql-psql-connection))
  (let ((process (emacsql-process connection)))
    (when (process-live-p process)
      (process-send-string process "\\q\n"))))

(defmethod emacsql-waiting-p ((connection emacsql-psql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (cond ((= (buffer-size) 1) (string= "]" (buffer-string)))
          ((> (buffer-size) 1) (string= "\n]"
                                        (buffer-substring
                                         (- (point-max) 2) (point-max)))))))

(defun emacsql-psql--check-error (connection)
  "Return non-nil or throw an appropriate error."
  (with-current-buffer (emacsql-buffer connection)
    (emacsql-wait connection)
    (setf (point) (point-min))
    (prog1 t
      (when (looking-at "ERROR:")
        (let ((message (buffer-substring (line-beginning-position)
                                          (line-end-position))))
          (emacsql-error "%s" message))))))

(defmethod emacsql ((connection emacsql-psql-connection) sql &rest args)
  (let ((sql-string (apply #'emacsql-compile sql args)))
    (emacsql-clear connection)
    (emacsql-send-string connection sql-string)
    (emacsql-psql--check-error connection)
    (emacsql-sqlite--parse connection)))

(provide 'emacsql-psql)

;;; emacsql-psql.el ends here
