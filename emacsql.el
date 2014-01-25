;;; emacsql.el --- high-level SQL database front-end -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacsql
;; Version: 1.0.0

;;; Commentary:

;; The purpose of this package is to provide a high-level Elisp
;; interface to a high-performance database back-end. Not every feature
;; of SQL will be exposed, but the important parts should be.

;; Most emacsql functions operate on a database connection. A
;; connection to SQLite is established with `emacsql-connect'. For
;; each such connection a sqlite3 inferior process is kept alive in
;; the background. Connections are closed with `emacsql-close'.

;;     (defvar db (emacsql-connect "company.db"))

;; Other types of database connections are available (PostgreSQL via
;; `emacsql-psql').

;; Use `emacsql' to send an s-expression SQL statements to a connected
;; database. Identifiers for tables and columns are symbols. SQL
;; keywords are lisp keywords. Anything else is data.

;;     (emacsql db [:create-table people [name id salary]])

;; Column constraints can optionally be provided in the schema.

;;     (emacsql db [:create-table people [name (id integer :unique) salary]])

;; Insert some values.

;;     (emacsql db [:insert :into people
;;                  :values (["Jeff"  1000 60000.0] ["Susan" 1001 64000.0])])

;; Currently all actions are synchronous and Emacs will block until
;; SQLite has indicated it is finished processing the last command.

;; Query the database for results:

;;     (emacsql db [:select [name id] :from employees :where (> salary 60000)])
;;     ;; => (("Susan" 1001))

;; Queries can be templates -- $1, $2, etc. -- so they don't need to
;; be built up dynamically:

;;     (emacsql db
;;              [:select [name id] :from employees :where (> salary $1)]
;;              50000)
;;     ;; => (("Jeff" 1000) ("Susan" 1001))

;; See README.md for much more complete documentation.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'emacsql-reap)
(require 'emacsql-compiler)

(defclass emacsql-connection ()
  ((process :type process
            :initarg :process
            :reader emacsql-process)
   (log-buffer :type (or null buffer)
               :initarg :log-buffer
               :accessor emacsql-log-buffer
               :documentation "Output log (debug).")
   (types :allocation :class
          :initform nil
          :reader emacsql-types
          :documentation "Maps Emacsql types to SQL types."))
  (:documentation "A connection to a SQL database.")
  :abstract t)

(defgeneric emacsql-close (connection)
  "Close CONNECTION and free all resources.")

(defgeneric emacsql-types (connection)
  "Return an alist mapping Emacsql types to database types.
This will mask `emacsql-type-map' during expression compilation.
This alist should have four key symbols: integer, float, object,
nil (default type). The values are strings to be inserted into a
SQL expression.")

(defmethod emacsql-buffer ((connection emacsql-connection))
  "Get proccess buffer for CONNECTION."
  (process-buffer (emacsql-process connection)))

(defmethod emacsql-log ((connection emacsql-connection) message)
  "Log MESSAGE into CONNECTION's log.
MESSAGE should not have a newline on the end."
  (let ((log (emacsql-log-buffer connection)))
    (when log
      (with-current-buffer log
        (setf (point) (point-max))
        (princ (concat message "\n") log)))))

;; Sending and receiving:

(defmethod emacsql-send-string
  ((connection emacsql-connection) string &optional no-log)
  "Send STRING to CONNECTION, automatically appending newline."
  (let ((process (emacsql-process connection)))
    (unless no-log (emacsql-log connection string))
    (process-send-string process string)
    (process-send-string process "\n")))

(defmethod emacsql-clear ((connection emacsql-connection))
  "Clear the process buffer for CONNECTION-SPEC."
  (with-current-buffer (emacsql-buffer connection)
    (erase-buffer)))

(defgeneric emacsql-waiting-p (connection)
  "Return non-nil if CONNECTION is ready for more input.")

(defmethod emacsql-wait ((connection emacsql-connection) &optional timeout)
  "Block until CONNECTION is waiting for further input."
  (let ((end (when timeout (+ (float-time) timeout))))
    (while (and (or (null timeout) (< (float-time) end))
                (not (emacsql-waiting-p connection)))
      (accept-process-output (emacsql-process connection) timeout))))

(defgeneric emacsql-parse (connection)
  "Return the results of parsing the latest output or signal an error.")

(defun emacsql-compile (connection sql &rest args)
  "Compile s-expression SQL for CONNECTION into a string."
  (let* ((mask (when connection (emacsql-types connection)))
         (emacsql-type-map (or mask emacsql-type-map)))
    (apply #'emacsql-format (emacsql-expand sql) args)))

(defmethod emacsql ((connection emacsql-connection) sql &rest args)
  "Send SQL s-expression to CONNECTION and return the results."
  (let ((sql-string (apply #'emacsql-compile connection sql args)))
    (emacsql-clear connection)
    (emacsql-send-string connection sql-string)
    (emacsql-wait connection)
    (emacsql-parse connection)))

;; Helper mixin class:

(defclass emacsql-simple-parser ()
  ()
  (:documentation
   "A mixin for back-ends with a straightforward output format.
The back-end prompt must be a single \"]\" character. This prompt
value was chosen because it is unreadable. Output must have
exactly one row per line, fields separated by whitespace. NULL
must display as \"nil\".")
  :abstract t)

(defmethod emacsql-waiting-p ((connection emacsql-simple-parser))
  "Return true of the end of the buffer has a properly-formatted prompt."
  (with-current-buffer (emacsql-buffer connection)
    (cond ((= (buffer-size) 1) (string= "]" (buffer-string)))
          ((> (buffer-size) 1) (string= "\n]"
                                        (buffer-substring
                                         (- (point-max) 2) (point-max)))))))

(defmethod emacsql-handle ((_ emacsql-simple-parser) message)
  "Signal a specific condition for MESSAGE from CONNECTION.
Subclasses should override this method in order to provide more
specific error conditions."
  (signal 'emacsql-syntax (list message)))

(defmethod emacsql-check-error ((connection emacsql-simple-parser))
  "Return the error message from CONNECTION, or nil for no error."
  (with-current-buffer (emacsql-buffer connection)
    (let ((case-fold-search t))
      (setf (point) (point-min))
      (when (looking-at "error:")
        (let* ((beg (line-beginning-position))
               (end (line-end-position)))
          (emacsql-handle connection (buffer-substring beg end)))))))

(defmethod emacsql-parse ((connection emacsql-simple-parser))
  "Parse well-formed output into an s-expression."
  (emacsql-check-error connection)
  (with-current-buffer (emacsql-buffer connection)
    (let ((standard-input (current-buffer)))
      (setf (point) (point-min))
      (cl-loop until (looking-at "]")
               collect (read) into row
               when (looking-at "\n")
               collect row into rows
               and do (progn (forward-char 1) (setf row ()))
               finally (cl-return rows)))))

(provide 'emacsql) ; end of generic function declarations

;; Automatic connection cleanup:

(defun emacsql-register (connection)
  "Register CONNECTION for automatic cleanup and return CONNECTION."
  (emacsql-reap-register connection #'emacsql-close (copy-sequence connection))
  connection)

;; Useful macros:

(require 'emacsql-sqlite) ; for `emacsql-connect'

(defmacro emacsql-with-connection (connection-spec &rest body)
  "Open an Emacsql connection, evaluate BODY, and close the connection.
CONNECTION-SPEC establishes a single binding.

  (emacsql-with-connection (db (emacsql-sqlite \"company.db\"))
    (emacsql db [:create-table foo [x]])
    (emacsql db [:insert :into foo :values ([1] [2] [3])])
    (emacsql db [:select * :from foo]))"
  (declare (indent 1))
  `(let ((,(car connection-spec) ,(cadr connection-spec)))
     (unwind-protect
         (progn ,@body)
       (emacsql-close ,(car connection-spec)))))

(defmacro emacsql-thread (connection &rest statements)
  "Thread CONNECTION through STATEMENTS.
A statement can be a list, containing a statement with its arguments."
  (declare (indent 1))
  `(let ((emacsql--conn ,connection))
     ,@(cl-loop for statement in statements
               when (vectorp statement)
               collect (list 'emacsql 'emacsql--conn statement)
               else
               collect (append (list 'emacsql 'emacsql--conn) statement))))

;; User interaction functions:

(defvar emacsql-show-buffer-name "*emacsql-show*"
  "Name of the buffer for displaying intermediate SQL.")

(defun emacsql--indent ()
  "Indent and wrap the SQL expression in the current buffer."
  (save-excursion
    (setf (point) (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp " [A-Z]+" nil :no-error)
        (when (> (current-column) (* fill-column 0.8))
          (backward-word)
          (insert "\n    "))))))

(defun emacsql-show-sql (string)
  "Fontify and display the SQL expression in STRING."
  (let ((fontified
         (with-temp-buffer
           (insert string)
           (sql-mode)
           (with-no-warnings ;; autoloaded by previous line
             (sql-highlight-sqlite-keywords))
           (font-lock-fontify-buffer)
           (emacsql--indent)
           (buffer-string))))
    (with-current-buffer (get-buffer-create emacsql-show-buffer-name)
      (if (< (length string) fill-column)
          (message "%s" fontified)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (insert fontified))
        (special-mode)
        (visual-line-mode)
        (pop-to-buffer (current-buffer))))))

(defun emacsql-flatten-sql (sql)
  "Convert a s-expression SQL into a flat string for display."
  (cl-destructuring-bind (string . vars) (emacsql-expand sql)
    (apply #'format string (cl-loop for i from 1 to (length vars)
                                    collect (intern (format "$%d" i))))))

;;;###autoload
(defun emacsql-show-last-sql (&optional prefix)
  "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer."
  (interactive "P")
  (let ((sql (emacsql-flatten-sql (preceding-sexp))))
    (if prefix
        (insert sql)
      (emacsql-show-sql ))))

;;; emacsql.el ends here
