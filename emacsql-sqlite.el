;;; emacsql-sqlite.el --- EmacSQL back-end for SQLite -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacsql
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (cl-lib "0.3") (emacsql "2.0.0"))

;;; Commentary:

;; During package installation EmacSQL will attempt to compile a
;; custom native binary for communicating with a SQLite database. If
;; this fails (a C compiler is not available), it will attempt to
;; download, with permission, a pre-built binary when the first
;; database connection is attempted.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'url)
(require 'url-http)
(require 'emacsql)
(require 'emacsql-system)

;;; Options

(defcustom emacsql-sqlite-automatic-fetch nil
  "If non-nil, the user will not be prompted to download the
pre-built SQLite binary. A value of `yes' will always approve the
download. A value of `no' will always deny the download."
  :group 'emacsql
  :type '(choice (const nil) (const yes) (const no)))

(defcustom emacsql-sqlite-automatic-build t
  "When non-nil, attempt to automatically build the SQLite binary locally.
When enabled, the C compiler build will be attempted on every
EmacSQL update, when the Elisp files are built. If it takes your
computer a long time to build the binary (e.g. 10 minutes on a
Raspberry Pi) it may be worth always fetching the pre-built
version."
  :group 'emacsql
  :type 'boolean)

;;; SQLite connection

(defvar emacsql-sqlite-data-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where EmacSQL is installed.")

(defvar emacsql-sqlite-executable
  (expand-file-name (format "bin/emacsql-sqlite-%s%s" (emacsql-system-tuple)
                            (if (memq system-type '(windows-nt cygwin ms-dos))
                                ".exe"
                              ""))
                    emacsql-sqlite-data-root)
  "Path to the EmacSQL backend (this is not the sqlite3 shell).")

(defvar emacsql-sqlite-reserved
  (emacsql-register-reserved
   '(ABORT ACTION ADD AFTER ALL ALTER ANALYZE AND AS ASC ATTACH
     AUTOINCREMENT BEFORE BEGIN BETWEEN BY CASCADE CASE CAST CHECK
     COLLATE COLUMN COMMIT CONFLICT CONSTRAINT CREATE CROSS
     CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP DATABASE DEFAULT
     DEFERRABLE DEFERRED DELETE DESC DETACH DISTINCT DROP EACH ELSE END
     ESCAPE EXCEPT EXCLUSIVE EXISTS EXPLAIN FAIL FOR FOREIGN FROM FULL
     GLOB GROUP HAVING IF IGNORE IMMEDIATE IN INDEX INDEXED INITIALLY
     INNER INSERT INSTEAD INTERSECT INTO IS ISNULL JOIN KEY LEFT LIKE
     LIMIT MATCH NATURAL NO NOT NOTNULL NULL OF OFFSET ON OR ORDER
     OUTER PLAN PRAGMA PRIMARY QUERY RAISE RECURSIVE REFERENCES REGEXP
     REINDEX RELEASE RENAME REPLACE RESTRICT RIGHT ROLLBACK ROW
     SAVEPOINT SELECT SET TABLE TEMP TEMPORARY THEN TO TRANSACTION
     TRIGGER UNION UNIQUE UPDATE USING VACUUM VALUES VIEW VIRTUAL WHEN
     WHERE WITH WITHOUT))
  "List of all of SQLite's reserved words.
http://www.sqlite.org/lang_keywords.html")

(defclass emacsql-sqlite-connection (emacsql-connection emacsql-protocol-mixin)
  ((file :initarg :file
         :type (or null string)
         :documentation "Database file name.")
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "INTEGER")
                      (float "REAL")
                      (object "TEXT")
                      (nil nil))))
  (:documentation "A connection to a SQLite database."))

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-connection) &key)
  (emacsql-sqlite-ensure-binary)
  (let* ((process-connection-type nil)  ; use a pipe
         (coding-system-for-write 'utf-8-auto)
         (coding-system-for-read 'utf-8-auto)
         (file (slot-value connection 'file))
         (buffer (generate-new-buffer " *emacsql-sqlite*"))
         (fullfile (if file (expand-file-name file) ":memory:"))
         (process (start-process
                   "emacsql-sqlite" buffer emacsql-sqlite-executable fullfile)))
    (setf (slot-value connection 'process) process)
    (setf (process-sentinel process)
          (lambda (proc _) (kill-buffer (process-buffer proc))))
    (emacsql-wait connection)
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2))
    (emacsql-register connection)))

(cl-defun emacsql-sqlite (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance 'emacsql-sqlite-connection :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-connection))
  "Gracefully exits the SQLite subprocess."
  (let ((process (emacsql-process connection)))
    (when (process-live-p process)
      (process-send-eof process))))

(cl-defmethod emacsql-send-message ((connection emacsql-sqlite-connection) message)
  (let ((process (emacsql-process connection)))
    (process-send-string process (format "%d " (string-bytes message)))
    (process-send-string process message)
    (process-send-string process "\n")))

(defvar emacsql-sqlite-condition-alist
  '(((1 4 9 12 17 18 20 21 22 25) emacsql-error)
    ((2)                          emacsql-internal)
    ((3 8 10 13 14 15 23)         emacsql-access)
    ((5 6)                        emacsql-locked)
    ((7)                          emacsql-memory)
    ((11 16 24 26)                emacsql-corruption)
    ((19)                         emacsql-constraint)
    ((27 28)                      emacsql-warning))
  "List of regexp's mapping sqlite3 output to conditions.")

(cl-defmethod emacsql-handle ((_ emacsql-sqlite-connection) code message)
  "Get condition for MESSAGE provided from SQLite."
  (signal
   (or (cl-second (cl-assoc code emacsql-sqlite-condition-alist :test #'memql))
       'emacsql-error)
   (list message)))

;;; SQLite compilation

(defun emacsql-sqlite-compile-switches ()
  "Return the compilation switches from the Makefile under sqlite/."
  (let ((makefile (expand-file-name "sqlite/Makefile" emacsql-sqlite-data-root))
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents makefile)
      (setf (point) (point-min))
      (cl-loop while (re-search-forward "-D[A-Z0-9_=]+" nil :no-error)
               collect (match-string 0)))))

(defun emacsql-sqlite-compile (&optional o-level async)
  "Compile the SQLite back-end for EmacSQL, returning non-nil on success.
If called with non-nil ASYNC the return value is meaningless."
  (let* ((cc (executable-find "cc"))
         (src (expand-file-name "sqlite" emacsql-sqlite-data-root))
         (files (mapcar (lambda (f) (expand-file-name f src))
                        '("sqlite3.c" "emacsql.c")))
         (cflags (list (format "-I%s" src) (format "-O%d" (or o-level 2))))
         (ldlibs (if (eq system-type 'windows-nt) () (list "-ldl")))
         (options (emacsql-sqlite-compile-switches))
         (output (list "-o" emacsql-sqlite-executable))
         (arguments (nconc cflags options files ldlibs output)))
    (cond ((not cc)
           (prog1 nil
             (message "Could not find C compiler, skipping SQLite build")))
          ((not emacsql-sqlite-automatic-build)
           (prog1 nil
             (message "Local SQLite build disabled, skipping")))
          (t (mkdir (expand-file-name "bin" emacsql-sqlite-data-root) t)
             (message "Compiling EmacSQL SQLite binary ...")
             (let ((log (get-buffer-create byte-compile-log-buffer)))
               (with-current-buffer log
                 (let ((inhibit-read-only t))
                   (insert (mapconcat #'identity (cons cc arguments) " ") "\n")
                   (eql 0 (apply #'call-process cc nil (if async 0 t) t
                                 arguments)))))))))

;;; SQLite binary fetching

(defvar emacsql-sqlite-user-prompted nil
  "To avoid prompting for fetch multiple times.")

(defvar emacsql-sqlite-host "http://nullprogram.s3.amazonaws.com/emacsql/"
  "Location where EmacSQL binaries can be found.")

(defun emacsql-sqlite-download (url filename)
  "Downlod URL to FILENAME, clobbering returning nil on failure.
This works like `url-copy-file' but actually checks for errors."
  (require 'url)
  (let ((buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (let ((response (url-http-parse-response)))
          (when (and (>= 200 response) (< response 300))
            (mkdir (file-name-directory filename) t)
            (let ((buffer-file-coding-system 'no-conversion))
              (write-region (1+ (with-no-warnings url-http-end-of-headers))
			    (point-max) filename)
              :success)))))))

(defun emacsql-sqlite-mark-exec (file)
  "Set executable bits on FILE's mode."
  (set-file-modes file (logior (file-modes file) #o111)))

(defun emacsql-sqlite-prompt ()
  "Ask the user if it's ok to download the SQLite binary."
  (let ((query "EmacSQL binary could not be built. Fetch from the Internet?"))
    (if emacsql-sqlite-automatic-fetch
        (not (eq emacsql-sqlite-automatic-fetch 'no))
      (unless emacsql-sqlite-user-prompted
        (let ((result (y-or-n-p query)))
          (setf emacsql-sqlite-user-prompted t)
          (when result
            (customize-save-variable 'emacsql-sqlite-automatic-fetch 'yes))
          result)))))

(defun emacsql-sqlite-fetch-binary ()
  "Fetch the SQLite binary from remote host."
  (let ((prompt (emacsql-sqlite-prompt)))
    (when prompt
      (let* ((file (file-name-nondirectory emacsql-sqlite-executable))
             (url (format "%s%s/%s"
                          emacsql-sqlite-host emacsql-version file)))
        (when (emacsql-sqlite-download url emacsql-sqlite-executable)
          (emacsql-sqlite-mark-exec emacsql-sqlite-executable)
          :success)))))

;;; Ensure the SQLite binary is available

(defun emacsql-sqlite-ensure-binary ()
  "Ensure the EmacSQL SQLite binary is available, signaling an error if not."
  (unless (file-exists-p emacsql-sqlite-executable)
    ;; try compiling at the last minute
    (unless (ignore-errors (emacsql-sqlite-compile 2))
      (unless (emacsql-sqlite-fetch-binary)
        (error "No EmacSQL SQLite binary available, aborting")))))

(provide 'emacsql-sqlite)

;;; emacsql-sqlite.el ends here
