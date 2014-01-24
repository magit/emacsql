;; emacsql-sqlite.el --- SQLite front-end for Emacsql -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'emacsql)

(defvar emacsql-sqlite3-executable "sqlite3"
  "Path to the sqlite3 executable.")

(defun emacsql-sqlite3-unavailable-p ()
  "Return a reason if the sqlite3 executable is not available.
:no-executable -- cannot find the executable
:cannot-execute -- cannot run the executable
:old-version -- sqlite3 version is too old"
  (let ((sqlite3 emacsql-sqlite3-executable))
    (if (null (executable-find sqlite3))
        :no-executable
      (condition-case _
          (with-temp-buffer
            (call-process sqlite3 nil (current-buffer) nil "--version")
            (let ((version (car (split-string (buffer-string)))))
              (if (version< version "3.0.0")
                  :old-version
                nil)))
        (error :cannot-execute)))))

(defclass emacsql-sqlite-connection (emacsql-connection emacsql-simple-parser)
  ((file :initarg :file
         :type (or null string)
         :documentation "Database file name."))
  (:documentation "A connection to a SQLite database."))

;;;###autoload
(cl-defun emacsql-sqlite (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let* ((buffer (generate-new-buffer "*emacsql-sqlite*"))
         (fullfile (if file (expand-file-name file) ":memory:"))
         (sqlite3 emacsql-sqlite3-executable)
         (process (start-process "emacsql-sqlite" buffer sqlite3
                                 "-interactive" fullfile)))
    (setf (process-sentinel process) (lambda (_proc _) (kill-buffer buffer)))
    (process-send-string process ".mode list\n")
    (process-send-string process ".separator ' '\n")
    (process-send-string process ".nullvalue nil\n")
    (process-send-string process ".prompt ]\n")
    (process-send-string process "EMACSQL;\n") ;; error message flush
    (let ((connection (make-instance
                       'emacsql-sqlite-connection
                       :process process
                       :file (when file fullfile)
                       :log-buffer )))
      (prog1 connection
        (when debug
          (setf (emacsql-log-buffer connection)
                (generate-new-buffer "*emacsql-log*")))
        (emacsql-wait connection)
        (emacsql-register connection)))))

;;;###autoload
(defalias 'emacsql-connect 'emacsql-sqlite)

(defmethod emacsql-close ((connection emacsql-sqlite-connection))
  "Gracefully exits the SQLite subprocess."
  (let ((process (emacsql-process connection)))
    (when (process-live-p process)
      (process-send-string process ".exit\n"))))

(defvar emacsql-sqlite-condition-alist
  '(("unable to open"              emacsql-access)
    ("cannot open"                 emacsql-access)
    ("source database is busy"     emacsql-access)
    ("unknown database"            emacsql-access)
    ("writable"                    emacsql-access)
    ("no such table"               emacsql-table)
    ("table [^ ]+ already exists"  emacsql-table)
    ("no such column"              emacsql-table)
    ("already another table"       emacsql-table)
    ("Cannot add"                  emacsql-table)
    ("table name"                  emacsql-table)
    ("already an index"            emacsql-table)
    ("constraint cannot be drop"   emacsql-table)
    ("database is locked"          emacsql-lock)
    ("no transaction is active"    emacsql-transaction)
    ("cannot start a transaction"  emacsql-transaction)
    ("out of memory"               emacsql-fatal)
    ("corrupt database"            emacsql-fatal)
    ("interrupt"                   emacsql-fatal)
    ("values were supplied"        emacsql-syntax)
    ("mismatch"                    emacsql-syntax)
    ("no such"                     emacsql-syntax)
    ("does not match"              emacsql-syntax)
    ("circularly defined"          emacsql-syntax)
    ("parameters are not allowed"  emacsql-syntax)
    ("missing"                     emacsql-syntax)
    ("is only allowed on"          emacsql-syntax)
    ("more than one primary key"   emacsql-syntax)
    ("not constant"                emacsql-syntax)
    ("duplicate"                   emacsql-syntax)
    ("name reserved"               emacsql-syntax)
    ("cannot use variables"        emacsql-syntax)
    ("no tables specified"         emacsql-syntax)
    ("syntax error"                emacsql-syntax)
    ("no such function"            emacsql-syntax)
    ("unknown function"            emacsql-syntax)
    ("wrong number of arguments"   emacsql-syntax)
    ("term does not match"         emacsql-syntax)
    ("clause"                      emacsql-syntax)
    ("tree is too large"           emacsql-syntax)
    ("too many"                    emacsql-syntax))
  "List of regexp's mapping sqlite3 output to conditions.")

(defun emacsql-sqlite-get-condition (message)
  "Get condition for MESSAGE provided from SQLite."
  (or (cadr (cl-assoc message emacsql-sqlite-condition-alist
                      :test (lambda (a b) (string-match-p b a))))
      'emacsql-error))

(defun emacsql-sqlite--check-error (conn)
  "Return non-nil or throw an appropriate error."
  (with-current-buffer (emacsql-buffer conn)
    (emacsql-wait conn)
    (setf (point) (point-min))
    (prog1 t
      (when (looking-at "Error:")
        (let* ((message (buffer-substring (line-beginning-position)
                                          (line-end-position)))
               (condition (emacsql-sqlite-get-condition message)))
          (signal condition (list message)))))))

(defmethod emacsql ((connection emacsql-sqlite-connection) sql &rest args)
  (let ((sql-string (apply #'emacsql-compile sql args)))
    (emacsql-clear connection)
    (emacsql-send-string connection sql-string)
    (emacsql-sqlite--check-error connection)
    (emacsql-simple-parse connection)))

(provide 'emacsql-sqlite)

;;; emacsql-sqlite.el ends here
