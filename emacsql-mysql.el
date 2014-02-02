;;; emacsql-mysql.el --- front-end for MySQL -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'emacsql)

(defvar emacsql-mysql-executable "mysql"
  "Path to the mysql command line executable.")

(defvar emacsql-mysql-sentinel "--------------\n\n--------------\n\n"
  "What MySQL will print when it has completed its output.")

(defclass emacsql-mysql-connection (emacsql-connection)
  ((dbname :reader emacsql-psql-dbname :initarg :dbname)
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "BIGINT")
                      (float "DOUBLE")
                      (object "LONGTEXT")
                      (nil "LONGTEXT")))))

(defun emacsql-mysql (dbname)
  "Connect to a MySQL server using the mysql command line program."
  (let* ((process-connection-type t)
         (buffer (generate-new-buffer " *emacsql-mysql*"))
         (mysql emacsql-mysql-executable)
         (command (mapconcat #'shell-quote-argument
                             (list mysql "-rfBNL" "--skip-pager" dbname)
                             " "))
         (process (start-process-shell-command
                   "emacsql-mysql" buffer (concat "stty raw &&" command)))
         (connection (make-instance 'emacsql-mysql-connection
                                    :process process
                                    :dbname dbname)))
    (setf (process-sentinel process)
          (lambda (proc _) (kill-buffer (process-buffer proc))))
    (emacsql connection [:set-session (= sql-mode 'NO_BACKSLASH_ESCAPES)])
    (emacsql-register connection)))

(defmethod emacsql-close ((connection emacsql-mysql-connection))
  (let ((process (emacsql-process connection)))
    (when (process-live-p process)
      (process-send-eof process))))

(defmethod emacsql-send-message ((connection emacsql-mysql-connection) message)
  (let ((process (emacsql-process connection)))
    (process-send-string process message)
    (process-send-string process "\\c\\p\n")))

(defmethod emacsql-waiting-p ((connection emacsql-mysql-connection))
  (let ((length (length emacsql-mysql-sentinel)))
    (with-current-buffer (emacsql-buffer connection)
      (and (>= (buffer-size) length)
           (progn (setf (point) (- (point-max) length))
                  (looking-at emacsql-mysql-sentinel))))))

(defmethod emacsql-parse ((connection emacsql-mysql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (let ((standard-input (current-buffer)))
      (setf (point) (point-min))
      (when (looking-at "ERROR")
        (search-forward ": ")
        (signal 'emacsql-error
                (list (buffer-substring (point) (line-end-position)))))
      (cl-loop until (looking-at emacsql-mysql-sentinel)
               collect (read) into row
               when (looking-at "\n")
               collect row into rows
               and do (setf row ())
               and do (forward-char)
               finally (cl-return rows)))))

(provide 'emacsql-mysql)

;;; emacsql-mysql.el ends here
