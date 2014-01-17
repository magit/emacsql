;;; emacsql.el --- SQL database built on SQLite -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacsql

;;; Commentary:

;; The purpose of this package is to provide a high-level Elisp
;; interface to a high-performance database backend. Not every last
;; feature of SQLite will be exposed at the high-level, but most of it
;; should be.

;; Every emacsql function operates on a database connection
;; established with `emacsql-connect', connecting to a SQLite database
;; file. For each connection a sqlite3 inferior process is kept alive.
;; Connections are closed with `elfeed-close'.

;;     (defvar db (emacsql-connect "company.db"))

;; Database connections are automatically closed when the connection
;; object is garbage collected. Though this doesn't excuse poor coding
;; habits! :-)

;; Table identifiers can be any lisp object: string, symbol, etc. I
;; suggest using a keyword. Use `emacsql-create' to create a table.

;;     (emacsql-create db :employees [name id salary])

;; Column constraints can optionally be provided.

;;     (emacsql-create db :employees [(name text) (id integer) (salary real)])

;; Insert values into a table with `emacsql-insert'.

;;     (emacsql-insert db :employees ["Jeff"  1000 60000.0]
;;                                   ["Susan" 1001 64000.0])

;; Currently all actions are synchronous and Emacs will block until
;; SQLite has indicated it is finished processing the last command.

;; High-level query construction is still a work-in-progress:

;;     (emacsql-select-raw db (concat "SELECT name, id FROM ':employees' "
;;                                    "WHERE salary > 60000;"))
;;     ;; => (("Susan" 1001))

;; Limitations:

;; Due to limitations of the SQLite command line program, emacsql is
;; *not* intended to play well with other programs accessing the
;; SQLite database. Text values and blobs are stored encoded as
;; s-expressions in order to avoid ambiguities in parsing output from
;; the command line. This is a high-performance database specifically
;; for Emacs.

;;; Code:

(require 'cl-lib)

(defvar emacsql-sqlite-executable "sqlite3"
  "Path to the sqlite3 executable.")

(cl-defstruct (emacsql (:constructor emacsql--create))
  "A connection to a SQLite database."
  process file log)

(defvar emacsql-connections ()
  "Collection of all known emacsql connections.
This collection exists for cleanup purposes.")

(defvar emacsql-reap-timer nil
  "Timer used to check for dead emacsql connections.")

(defun emacsql--ref (thing)
  "Create a weak reference to THING."
  (let ((ref (make-hash-table :test 'eq :size 1 :weakness 'value)))
    (prog1 ref
      (setf (gethash t ref) thing))))

(defun emacsql--deref (ref)
  "Retrieve value from REF."
  (gethash t ref))

(cl-defun emacsql-connect (file &key log)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:log LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (emacsql-start-reap-timer)
  (let* ((buffer (generate-new-buffer "*emacsql-connection*"))
         (process (start-process "emacsql" buffer emacsql-sqlite-executable
                                 (or file ":memory:"))))
    (setf (process-sentinel process) (lambda (_proc _) (kill-buffer buffer)))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (process-send-string process ".prompt #\n")
    (process-send-string process ".mode line\n")
    (process-send-string process ".nullvalue nil\n")
    (let ((conn (emacsql--create :process process :file file)))
      (when log
        (setf (emacsql-log conn) (generate-new-buffer "*emacsql-log*")))
      (prog1 conn
        (push (cons (copy-seq conn) (emacsql--ref conn))
              emacsql-connections)))))

(defun emacsql-close (conn)
  "Close connection to CONN database."
  (let ((process (emacsql-process conn)))
    (when (and process (process-live-p process))
      (process-send-string process ".exit\n"))))

(defun emacsql-buffer (conn)
  "Get proccess buffer for CONN."
  (process-buffer (emacsql-process conn)))

(defun emacsql-reap ()
  "Clean up after lost connections."
  (cl-loop for (conn-copy . ref) in emacsql-connections
           when (null (emacsql--deref ref))
           count (prog1 t (ignore-errors (emacsql-close emacsql-copy)))
           into total
           else collect (cons conn-copy ref) into connections
           finally (progn
                     (setf emacsql-connections connections)
                     (return total))))

(cl-defun emacsql-start-reap-timer (&optional (interval 60))
  "Start the automatic `emacql-reap' timer."
  (unless emacsql-reap-timer
    (setf emacsql-reap-timer (run-at-time interval interval #'emacsql-reap))))

(defun emacsql-stop-reap-timer ()
  "Stop the automatic `emacsql-reap' timer."
  (when (timerp emacsql-reap-timer)
    (cancel-timer emacsql-reap-timer)
    (setf emacsql-reap-timer nil)))

(defun emacsql--log (conn &rest messages)
  (let ((log (emacsql-log conn)))
    (when log
      (with-current-buffer log
        (setf (point) (point-max))
        (mapc (lambda (s) (princ s log)) messages)))))

(defun emacsql--send (conn string)
  "Send STRING to CONN, automatically appending newline."
  (let ((process (emacsql-process conn)))
    (emacsql--log conn string "\n")
    (process-send-string process string)
    (process-send-string process "\n")))

(defun emacsql--clear (conn)
  "Clear the process buffer for CONN."
  (with-current-buffer (emacsql-buffer conn)
    (erase-buffer)))

(defun emacsql--complete-p (conn)
  "Return non-nil if receive buffer has finished filling."
  (with-current-buffer (emacsql-buffer conn)
    (cond ((= (buffer-size) 1) (string= "#" (buffer-string)))
          ((> (buffer-size) 1) (string= "\n#"
                                        (buffer-substring
                                         (- (point-max) 2) (point-max)))))))

(defun emacsql--parse (conn &rest named)
  "Parse a query result into an s-expression.
If NAMED is non-nil, don't include column names."
  (with-current-buffer (emacsql-buffer conn)
    (let ((standard-input (current-buffer)))
      (setf (point) (point-min))
      (cl-loop until (looking-at "#")
               for name = (read)
               do (forward-char 3)
               for value = (read)
               when named collect (cons name value) into row
                     else collect value into row
               do (forward-char)
               when (or (looking-at "\n") (looking-at "#"))
               collect row into rows and do (setf row ())
               finally (return rows)))))

(defun emacsql-escape (identifier &optional force)
  "Escape an identifier, always with quotes when FORCE is non-nil."
  (let ((string (if (stringp identifier)
                    identifier
                  (format "%S" identifier)))
        (forbidden "[]-\000-\040!\"#%&'()*+,./:;<=>?@[\\^`{|}~\177]"))
    (when (string-match-p "\n" string)
      (error "Newlines not permitted in identifiers by emacsql."))
    (if (or force
            (string-match-p forbidden string)
            (string-match-p "^[0-9$]" string))
        (format "'%s'" (replace-regexp-in-string "'" "''" string))
      string)))

(defun emacsql--check-error (conn)
  "Return non-nil or throw an appropriate error."
  (with-current-buffer (emacsql-buffer conn)
    (emacsql-wait conn)
    (setf (point) (point-min))
    (prog1 t
      (when (looking-at "Error:")
        (error (buffer-substring (line-beginning-position)
                                 (line-end-position)))))))

(defun emacsql-wait (conn &optional timeout)
  "Block Emacs until CONN has finished sending output."
  (while (not (emacsql--complete-p conn))
    (accept-process-output (emacsql-process conn))))

(defmacro emacsql-with-errors (conn &rest body)
  "Run BODY checking for errors from SQLite after completion."
  (declare (indent 1))
  `(progn
     (emacsql--clear ,conn)
     ,@body
     (emacsql--check-error ,conn)))

(defun emacsql-create (conn table schema &optional if-not-exists)
  "Create TABLE in CONN with SCHEMA."
  (emacsql-with-errors conn
    (cl-loop for column being the elements of schema
             when (consp column)
             collect (mapconcat #'emacsql-escape column " ")
             into parts
             else collect (format "%s" column) into parts
             finally (emacsql--send
                      conn
                      (format "CREATE TABLE %s%s(%s);"
                              (if if-not-exists "IF NOT EXISTS " "")
                              (emacsql-escape table)
                              (mapconcat #'identity parts ", "))))))

(defun emacsql-drop (conn table)
  "Drop TABLE from CONN."
  (emacsql-with-errors conn
    (emacsql--send conn (format "DROP TABLE %s;" (emacsql-escape table)))))

(defun emacsql-escape-value (value)
  "Escape VALUE for sending to SQLite."
  (let ((print-escape-newlines t))
    (cond ((null value) "NULL")
          ((numberp value) (prin1-to-string value))
          (:else (emacsql-escape (prin1-to-string value) t)))))

(defun emacsql-insert (conn table &rest rows)
  "Insert ROWS into TABLE.
Each row must be a sequence of values to store into TABLE.

  (emacsql-insert db :table '(\"Chris\" 0) [\"Jeff\" 1])"
  (emacsql-with-errors conn
    (emacsql--send
     conn
     (format "INSERT INTO %s VALUES (%s);" (emacsql-escape table)
             (mapconcat (lambda (row)
                          (mapconcat #'emacsql-escape-value row ", "))
                        rows "), (")))))

(defun emacsql-select-raw (conn query)
  "Send a raw QUERY string to CONN."
  (emacsql--clear conn)
  (emacsql--send conn query)
  (emacsql-wait conn)
  (emacsql--check-error conn)
  (emacsql--parse conn))

(provide 'emacsql)

;;; emacsql.el ends here
