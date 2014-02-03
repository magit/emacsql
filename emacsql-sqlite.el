;; emacsql-sqlite.el --- SQLite front-end for EmacSQL -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'emacsql)
(require 'emacsql-system)

(defvar emacsql-sqlite-executable
  (expand-file-name (concat "bin/emacsql-sqlite-" (emacsql-system-tuple))
                    (file-name-directory load-file-name))
  "Path to the EmacSQL backend (this is not the sqlite3 shell).")

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

(cl-defun emacsql-sqlite (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let* ((process-connection-type nil)  ; use a pipe
         (coding-system-for-write 'utf-8-auto)
         (coding-system-for-read 'utf-8-auto)
         (buffer (generate-new-buffer " *emacsql-sqlite*"))
         (fullfile (if file (expand-file-name file) ":memory:"))
         (process (start-process
                   "emacsql-sqlite" buffer emacsql-sqlite-executable fullfile))
         (connection (make-instance 'emacsql-sqlite-connection
                                    :process process
                                    :file (when file fullfile))))
    (setf (process-sentinel process)
          (lambda (proc _) (kill-buffer (process-buffer proc))))
    (emacsql-wait connection)
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2))
    (when debug (emacsql-enable-debugging connection))
    (emacsql-register connection)))

(defmethod emacsql-close ((connection emacsql-sqlite-connection))
  "Gracefully exits the SQLite subprocess."
  (let ((process (emacsql-process connection)))
    (when (process-live-p process)
      (process-send-eof process))))

(defmethod emacsql-send-message ((connection emacsql-sqlite-connection) message)
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

(defmethod emacsql-handle ((_ emacsql-sqlite-connection) code message)
  "Get condition for MESSAGE provided from SQLite."
  (signal
   (or (cl-second (cl-assoc code emacsql-sqlite-condition-alist :test #'memql))
       'emacsql-error)
   (list message)))

(provide 'emacsql-sqlite)

;;; emacsql-sqlite.el ends here
