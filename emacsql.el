;;; emacsql.el --- SQL database built on SQLite -*- lexical-binding: t; -*-

;;; Commentary:

;; The purpose of this package is to provide a high-level Elisp
;; interface to a high-performance database backend. Not every last
;; feature of SQLite will be exposed at the high-level, but most of it
;; should be.

;; Every emacsql function operates on a database connection
;; established with `emacsql-connect', connecting to a SQLite database
;; file. For each connection a sqlite3 inferior process is kept alive.
;; Connections are closed with `elfeed-close'.

;;     (defvar db (emacsql-connect "company.sqlite"))

;; Database connections are automatically closed when the connection
;; object is garbage collected. Though this doesn't excuse poor coding
;; habits! :-)

;; Table identifiers can be any lisp object: string, symbol, etc. I
;; suggest using a keyword. Use `emacsql-create' to create a table.

;;     (emacsql-create db :employees '(name id salary))

;; Type information can optionally be provided.

;;     (emacsql-create db :employees '((name text) (id integer) salary))

;; Insert values into a table with `emacsql-insert'.

;;     (emacsql-insert db :employees "Jeff"  1000 60000)
;;     (emacsql-insert db :employees "Susan" 1001 64000)

;; Currently all actions are synchronous and Emacs will block until
;; SQLite has indicated it is finished processing the last command.

;; High-level query construction is still a work-in-progress:

;;     (emacsql-select-raw db (concat "SELECT name, id FROM ':employees' "
;;                                    "WHERE salary > 60000;"))
;;     ;; => (((name . "Susan") (id . 1001)))

;; Limitations:

;; Due to limitations of the SQLite command line program, emacsql is
;; *not* intended to play well with other programs accessing the
;; SQLite database. Text values and blobs are stored encoded as
;; s-expressions in order to avoid ambiguities in parsing output from
;; the command line. This is a high-performance database specifically
;; for Emacs.

;;; Code:

(require 'cl-lib)

(defvar sqlite-program-name "sqlite3"
  "Path to the sqlite3 executable.")

(cl-defstruct (emacsql (:constructor emacsql--create))
  process file closed-p)

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

(defun emacsql-connect (file)
  "Open a connected to database stored in FILE."
  (emacsql-start-reap-timer)
  (let* ((buffer (generate-new-buffer "emacsql-connection"))
         (process (start-process "emacsql" buffer sqlite-program-name file)))
    (process-send-string process ".prompt #\n")
    (process-send-string process ".mode line\n")
    (let ((emacsql (emacsql--create :process process :file file)))
      (prog1 emacsql
        (push (cons (copy-seq emacsql) (emacsql--ref emacsql))
              emacsql-connections)))))

(defun emacsql-close (emacsql)
  "Close connection to EMACSQL database."
  (let ((process (emacsql-process emacsql)))
    (when (and process (process-live-p process))
      (if (emacsql-closed-p emacsql)
          (kill-process process)
        (setf (emacsql-closed-p emacsql) t)
        (process-send-string process ".exit\n")))))

(defun emacsql-buffer (emacsql)
  "Get proccess buffer for EMACSQL."
  (process-buffer (emacsql-process emacsql)))

(defun emacsql-reap ()
  "Clean up after lost connections."
  (cl-loop for (emacsql-copy . ref) in emacsql-connections
           when (null (emacsql--deref ref))
           count (prog1 t (emacsql-close emacsql-copy)) into total
           else collect (cons emacsql-copy ref) into connections
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

(defun emacsql--send (emacsql string)
  "Send STRING to EMACSQL, automatically appending newline."
  (let ((process (emacsql-process emacsql)))
    (process-send-string process string)
    (process-send-string process "\n")))

(defun emacsql--clear (emacsql)
  "Clear the process buffer for EMACSQL."
  (with-current-buffer (emacsql-buffer emacsql)
    (erase-buffer)))

(defun emacsql--complete-p (emacsql)
  "Return non-nil if receive buffer has finished filling."
  (with-current-buffer (emacsql-buffer emacsql)
    (cond ((= (buffer-size) 1) (string= "#" (buffer-string)))
          ((> (buffer-size) 1) (string= "\n#"
                                        (buffer-substring
                                         (- (point-max) 2) (point-max)))))))

(defun emacsql--parse (emacsql)
  "Parse a query result into an s-expression."
  (with-current-buffer (emacsql-buffer emacsql)
    (let ((standard-input (current-buffer)))
      (setf (point) (point-min))
      (cl-loop until (looking-at "#")
               for (name _= value) = (list (read) (read) (read))
               collect (cons name value) into row
               do (forward-char)
               when (or (looking-at "\n") (looking-at "#"))
               collect row into rows and do (setf row ())
               finally (return rows)))))

(defun emacsql-escape (identifier)
  "Escape an identifier."
  (let ((string (if (stringp identifier)
                    identifier
                  (format "%S" identifier))))
    (when (string-match-p "\n" string)
      (error "Newlines not permitted in identifiers by emacsql."))
    (if (or (string-match-p "[]-\000-\040!\"#%&'()*+,./:;<=>?@[\\^`{|}~\177]"
                            string)
            (string-match-p "^[0-9$]" string))
        (format "\"%s\"" (replace-regexp-in-string "\"" "\"\"" string))
      string)))

(defun emacsql--check-error (emacsql)
  "Return non-nil or throw an appropriate error."
  (with-current-buffer (emacsql-buffer emacsql)
    (emacsql-wait emacsql)
    (setf (point) (point-min))
    (prog1 t
      (when (looking-at "Error:")
        (error (buffer-substring (line-beginning-position)
                                 (line-end-position)))))))

(defun emacsql-wait (emacsql &optional timeout)
  "Block Emacs until EMACSQL has finished sending output."
  (while (not (emacsql--complete-p emacsql))
    (accept-process-output (emacsql-process emacsql))))

(defmacro emacsql-with-errors (emacsql &rest body)
  "Run BODY checking for errors from SQLite after completion."
  (declare (indent 1))
  `(progn
     (emacsql--clear ,emacsql)
     ,@body
     (emacsql--check-error ,emacsql)))

(defun emacsql-create (emacsql table spec &optional if-not-exists)
  "Create TABLE in EMACSQL with SPEC."
  (emacsql-with-errors emacsql
    (cl-loop for column in spec
             when (consp column)
             collect (mapconcat #'emacsql-escape column " ")
             into parts
             else collect (format "%s" column) into parts
             finally (emacsql--send
                      emacsql
                      (format "CREATE TABLE %s%s(%s);"
                              (if if-not-exists "IF NOT EXISTS " "")
                              (emacsql-escape table)
                              (mapconcat #'identity parts ", "))))))

(defun emacsql-drop (emacsql table)
  "Drop TABLE from EMACSQL."
  (emacsql-with-errors emacsql
    (emacsql--send emacsql (format "DROP TABLE %s;" (emacsql-escape table)))))

(defun emacsql-escape-value (value)
  "Escape VALUE for sending to SQLite."
  (let ((print-escape-newlines t))
    (if (numberp value)
        (prin1-to-string value)
      (emacsql-escape (prin1-to-string value)))))

(defun emacsql-insert (emacsql table &rest values)
  "Insert VALUES into TABLE."
  (emacsql-with-errors emacsql
    (emacsql--send emacsql
                   (format "INSERT INTO %s VALUES(%s);"
                           (emacsql-escape table)
                           (mapconcat #'emacsql-escape-value values ", ")))))

(defun emacsql-select-raw (emacsql query)
  "Send a raw QUERY string to EMACSQL."
  (emacsql--clear emacsql)
  (emacsql--send emacsql query)
  (emacsql-wait emacsql)
  (emacsql--check-error emacsql)
  (emacsql--parse emacsql))

(provide 'emacsql)

;;; emacsql.el ends here
