;;; emacsql.el --- SQL database built on SQLite -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacsql
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.3"))

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
;; suggest using a symbol. Use `emacsql-create' to create a table.

;;     (emacsql db [:create-table people [name id salary]])

;; Column constraints can optionally be provided.

;;     (emacsql db [:create-table people [name (id integer :unique) salary]])

;; Insert values into a table with `emacsql-insert'.

;;     (emacsql db [:insert :into people
;;                  :values (["Jeff"  1000 60000.0] ["Susan" 1001 64000.0])])

;; Currently all actions are synchronous and Emacs will block until
;; SQLite has indicated it is finished processing the last command.

;; Query the database for results:

;;     (emacsql db [:select [name id] :from employees :where (> salary 60000)])
;;     ;; => (("Susan" 1001))

;; Queries can be templates using $1, $2, etc.:

;;     (emacsql db
;;              [:select [name id] :from employees :where (> salary $1)]
;;              50000)
;;     ;; => (("Jeff" 1000) ("Susan" 1001))

;; Limitations:

;; Due to limitations of the SQLite command line program, emacsql is
;; *not* intended to play well with other programs accessing the
;; SQLite database. Text values and blobs are stored encoded as
;; s-expressions in order to avoid ambiguities in parsing output from
;; the command line. This is a high-performance database specifically
;; for Emacs.

;;; Code:

(require 'cl-lib)

(defvar emacsql-sqlite3-executable "sqlite3"
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

(defun emacsql--flush (conn)
  "Flush (and toss) any waiting output from CONN."
  (emacsql--send conn ";\n.print EMACSQL")
  (with-current-buffer (emacsql-buffer conn)
    (cl-loop until (string-match-p "EMACSQL\n#" (buffer-string))
             do (accept-process-output)))
  (emacsql--clear conn))

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
              (if (version< version "3.7.15")
                  :old-version
                nil)))
        (error :cannot-execute)))))

(cl-defun emacsql-connect (file &key log)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:log LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (emacsql-start-reap-timer)
  (let* ((buffer (generate-new-buffer "*emacsql-connection*"))
         (fullfile (if file (expand-file-name file) ":memory:"))
         (process (start-process "emacsql" buffer emacsql-sqlite3-executable
                                 "-interactive" fullfile)))
    (setf (process-sentinel process) (lambda (_proc _) (kill-buffer buffer)))
    (process-send-string process ".prompt #\n")
    (process-send-string process ".mode line\n")
    (process-send-string process ".nullvalue nil\n")
    (let ((conn (emacsql--create :process process :file (when file fullfile))))
      (when log
        (setf (emacsql-log conn) (generate-new-buffer "*emacsql-log*")))
      (prog1 conn
        (emacsql--flush conn)
        (push (cons (copy-sequence conn) (emacsql--ref conn))
              emacsql-connections)))))

(defun emacsql-close (conn)
  "Close connection to CONN database."
  (let ((process (emacsql-process conn)))
    (when (and process (process-live-p process))
      (process-send-string process ".exit\n"))))

(defmacro emacsql-with-connection (conn-spec &rest body)
  "Open an Emacsql connection, evaluate BODY, and close the connection.
CONN-SPEC is a connection specification like the call to
`emacsql-connect', establishing a single binding.

  (emacsql-with-connection (db \"company.db\")
    (emacsql db [:create-table foo [x]])
    (emacsql db [:insert :into foo :values ([1] [2] [3])])
    (emacsql db [:select * :from foo]))"
  (declare (indent 1))
  `(let ((,(car conn-spec) (emacsql-connect ,@(cdr conn-spec))))
     (unwind-protect
         (progn ,@body)
       (emacsql-close ,(car conn-spec)))))

(defmacro emacsql-thread (conn &rest statements)
  "Thread CONN through STATEMENTS.
A statement can be a list, containing a statement with its arguments."
  (declare (indent 1))
  `(let ((emacsql--conn ,conn))
     ,@(cl-loop for statement in statements
               when (vectorp statement)
               collect (list 'emacsql 'emacsql--conn statement)
               else
               collect (append (list 'emacsql 'emacsql--conn) statement))))

(defun emacsql-buffer (conn)
  "Get proccess buffer for CONN."
  (process-buffer (emacsql-process conn)))

(defun emacsql-reap ()
  "Clean up after lost connections."
  (cl-loop for (conn-copy . ref) in emacsql-connections
           when (null (emacsql--deref ref))
           count (prog1 t (ignore-errors (emacsql-close conn-copy)))
           into total
           else collect (cons conn-copy ref) into connections
           finally (progn
                     (setf emacsql-connections connections)
                     (cl-return total))))

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
  "Log MESSAGES into CONN's log."
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

(defun emacsql--parse (conn)
  "Parse a query result into an s-expression."
  (with-current-buffer (emacsql-buffer conn)
    (let ((standard-input (current-buffer)))
      (setf (point) (point-min))
      (cl-loop until (looking-at "#")
               do (search-forward " = ")
               for value = (read)
               collect value into row
               do (forward-char)
               when (or (looking-at "\n") (looking-at "#"))
               collect row into rows and do (setf row ())
               finally (cl-return rows)))))

(defun emacsql-quote (string)
  "Quote STRING for use in a SQL expression."
  (format "'%s'" (replace-regexp-in-string "'" "''" string)))

(defun emacsql-escape-identifier (identifier)
  "Escape an identifier, always with quotes when FORCE is non-nil."
  (let ((string (cl-typecase identifier
                  (string identifier)
                  (keyword (substring (symbol-name identifier) 1))
                  (otherwise (format "%S" identifier))))
        (forbidden "[]-\000-\040!\"#%&'()*+,./;<=>?@[\\^`{|}~\177]"))
    (when (or (string-match-p forbidden string)
              (string-match-p "^[0-9$]" string))
      (error "Invalid Emacsql identifier: %S" identifier))
    (if (string-match-p ":" string)
        (replace-regexp-in-string ":" "." string)
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
  (let ((end (when timeout (+ (float-time) timeout))))
    (while (and (or (null timeout) (< (float-time) end))
                (not (emacsql--complete-p conn)))
      (accept-process-output (emacsql-process conn) timeout))))

(defun emacsql-escape-value (value)
  "Escape VALUE for sending to SQLite."
  (let ((print-escape-newlines t))
    (cond ((null value) "NULL")
          ((numberp value) (prin1-to-string value))
          ((emacsql-quote (prin1-to-string value))))))

(defun emacsql-escape-vector (vector)
  "Encode VECTOR into a SQL vector scalar."
  (cl-etypecase vector
    (list   (mapconcat #'emacsql-escape-vector vector ", "))
    (vector (concat "(" (mapconcat #'emacsql-escape-value vector ", ") ")"))))

;; SQL Expansion:

(defvar emacsql-expanders ()
  "Alist of all expansion functions.")

(defvar emacsql-expander-cache (make-hash-table :test 'equal)
  "Cache used to memoize `emacsql-expand'.")

(defun emacsql-add-expander (keyword arity function)
  "Register FUNCTION for KEYWORD as a SQL expander.
FUNCTION should accept the keyword's arguments and should return
a list of (<string> [arg-pos] ...)."
  (prog1 keyword
    (when emacsql-expander-cache (clrhash emacsql-expander-cache))
    (push (list keyword arity function) emacsql-expanders)))

(defmacro emacsql-defexpander (keyword args &rest body)
  "Define an expander for KEYWORD."
  (declare (indent 2))
  `(emacsql-add-expander ,keyword ,(length args) (lambda ,args ,@body)))

(defun emacsql-expand (sql &optional subsql-p)
  "Expand SQL into a SQL-consumable string, with variables."
  (let* ((cache emacsql-expander-cache)
         (cached (and cache (gethash sql cache))))
    (or cached
        (cl-loop with items = (cl-coerce sql 'list)
                 while (not (null items))
                 for keyword = (pop items)
                 for (arity expander) = (cdr (assoc keyword emacsql-expanders))
                 when expander
                 collect (apply expander (cl-subseq items 0 arity)) into parts
                 else do (error "Unrecognized keyword %s" keyword)
                 do (setf items (cl-subseq items arity))
                 finally
                 (let ((string (concat (if subsql-p "(" "")
                                       (mapconcat #'car parts " ")
                                       (if subsql-p ")" ";")))
                       (vars (apply #'nconc (mapcar #'cdr parts))))
                   (cl-return (if cache
                                  (setf (gethash sql cache) (cons string vars))
                                (cons string vars))))))))

(defun emacsql-format (expansion &rest args)
  "Fill in the variables EXPANSION with ARGS."
  (cl-destructuring-bind (format . vars) expansion
    (unless (= (length args) (length vars))
      (error "Wrong number of arguments for SQL template."))
    (apply #'format format
           (cl-loop for (i . kind) in vars collect
                    (let ((thing (nth i args)))
                      (cl-ecase kind
                        (:identifier (emacsql-escape-identifier thing))
                        (:value (emacsql-escape-value thing))
                        (:vector (emacsql-escape-vector thing))
                        (:schema (car (emacsql--schema-to-string thing)))
                        (:auto (if (symbolp thing)
                                   (emacsql-escape-identifier thing)
                                 (emacsql-escape-value thing)))))))))

(defun emacsql-compile (sql &rest args)
  "Compile structured SQL expression into a string."
  (apply #'emacsql-format (emacsql-expand sql) args))

(defun emacsql (conn sql &rest args)
  "Send structured SQL expression to CONN with ARGS."
  (emacsql--clear conn)
  (emacsql--send conn (apply #'emacsql-format (emacsql-expand sql) args))
  (emacsql--check-error conn)
  (emacsql--parse conn))

(defun emacsql-var (var)
  "Return the index number of VAR, or nil if VAR is not a variable.
A variable is a symbol that looks like $1, $2, $3, etc. A $ means $1."
  (when (symbolp var)
    (let ((name (symbol-name var)))
      (when (eql (aref name 0) ?$)
        (if (> (length name) 1)
            (1- (read (substring name 1)))
          0)))))

(defun emacsql-escape-format (thing &optional kind)
  "Escape THING for use as a `format' spec, pre-escaping for KIND.
KIND should be :value or :identifier."
  (replace-regexp-in-string
   "%" "%%" (cl-case kind
              (:value (emacsql-escape-value thing))
              (:identifier (emacsql-escape-identifier thing))
              (:vector (emacsql-escape-vector thing))
              (otherwise thing))))

(defvar emacsql--vars ()
  "For use with `emacsql-with-vars'.")

(defun emacsql--vars-var (thing kind)
  "Only use within `emacsql-with-vars'!"
  (if (emacsql-var thing)
      (prog1 "%s" (push (cons (emacsql-var thing) kind) emacsql--vars))
    (cl-ecase kind
      ((:identifier :value :vector) (emacsql-escape-format thing kind))
      (:auto (emacsql-escape-format
              thing (if (symbolp thing) :identifier :value))))))

(defun emacsql--vars-combine (expanded)
  "Only use within `emacsql-with-vars'!"
  (cl-destructuring-bind (string . vars) expanded
    (setf emacsql--vars (nconc emacsql--vars vars))
    string))

(defmacro emacsql-with-vars (prefix &rest body)
  "Evaluate BODY, collecting variables with `var', `combine', `expr', `idents'.
BODY should return a string, which will be combined with variable
definitions for return from a `emacsql-defexpander'."
  (declare (indent 1))
  `(let ((emacsql--vars ()))
     (cl-flet* ((var (thing kind) (emacsql--vars-var thing kind))
                (combine (expanded) (emacsql--vars-combine expanded))
                (expr (thing) (combine (emacsql--expr thing)))
                (idents (thing) (combine (emacsql--idents thing)))
                (subsql (thing) (combine (emacsql-expand thing t))))
       (cons (concat ,prefix (progn ,@body)) emacsql--vars))))

(defun emacsql--column-to-string (column)
  "Convert COLUMN schema into a SQL string."
  (emacsql-with-vars ""
    (let ((name (var (pop column) :identifier))
          (output ())
          (type nil))
      (while column
        (let ((next (pop column)))
          (cl-case next
            (:primary (push "PRIMARY KEY" output))
            (:non-nil (push "NOT NULL" output))
            (:unique  (push "UNIQUE" output))
            (:default (push "DEFAULT" output)
                      (push (var (pop column) :value) output))
            (:check   (push "CHECK" output)
                      (push (format "(%s)" (expr (pop column))) output))
            (integer  (setf type "INTEGER"))
            (float    (setf type "REAL"))
            (object   (setf type "TEXT"))
            (otherwise
             (if (keywordp next)
                 (error "Unknown schema contraint %s" next)
               (error "Invalid type %s: %s" next
                      "must be 'integer', 'float', or 'object'"))))))
      (setf output (nreverse output))
      (when type (push type output))
      (push name output)
      (mapconcat #'identity output " "))))

(defun emacsql--columns-to-string (columns)
  "Convert COLUMNS into a SQL-consumable string."
  (emacsql-with-vars ""
    (cl-loop for column across columns
             when (symbolp column)
             collect (var column :identifier) into parts
             else
             collect (combine (emacsql--column-to-string column)) into parts
             finally (cl-return (mapconcat #'identity parts ", ")))))

(defun emacsql--foreign-key (spec)
  (emacsql-with-vars "FOREIGN KEY "
    (cl-destructuring-bind (child table parent . actions) (cl-coerce spec 'list)
      (mapconcat
       #'identity
       (cons
        (format "(%s) REFERENCES %s (%s)" (idents child) (var table :identifier)
                (idents parent))
        (cl-loop for (key value) on actions by #'cddr collect
                 (cl-ecase key
                   (:on-update "ON UPDATE")
                   (:on-delete "ON DELETE"))
                 collect
                 (cl-ecase value
                   (:restrict "RESTRICT")
                   (:set-nil "SET NULL")
                   (:set-default "SET DEFAULT")
                   (:cascade "CASCADE"))))
       " "))))

(defun emacsql--schema-to-string (schema)
  (cl-etypecase schema
    (vector (emacsql--columns-to-string schema))
    (list
     (emacsql-with-vars ""
       (mapconcat
        #'identity
        (cons
         (combine (emacsql--columns-to-string (pop schema)))
         (cl-loop for (key value) on schema by #'cddr collect
                  (cl-ecase key
                    (:primary (format "PRIMARY KEY (%s)" (idents value)))
                    (:unique (format "UNIQUE (%s)" (idents value)))
                    (:check (format "CHECK (%s)" (expr value)))
                    (:foreign (combine (emacsql--foreign-key value))))))
        ", ")))))

(defun emacsql--vector (vector)
  "Expand VECTOR, making variables as needed."
  (emacsql-with-vars ""
    (cl-etypecase vector
      (symbol
       (var vector :vector))
      (list
       (mapconcat (lambda (v) (combine (emacsql--vector v))) vector ", "))
      (vector
       (format "(%s)" (mapconcat (lambda (x) (var x :value)) vector ", "))))))

(defun emacsql--expr (expr)
  "Expand EXPR recursively."
  (emacsql-with-vars ""
    (if (atom expr)
        (var expr :auto)
      (cl-destructuring-bind (op . args) expr
        (cl-flet ((recur (n) (combine (emacsql--expr (nth n args)))))
          (cl-ecase op
            ;; Trinary/binary
            ((<= >=)
             (cl-ecase (length args)
               (2 (format "%s %s %s" (recur 0) op (recur 1)))
               (3 (format "%s BETWEEN %s AND %s"
                          (recur 1)
                          (recur (if (eq op '>=) 2 0))
                          (recur (if (eq op '>=) 0 2))))))
            ;; Binary
            ((< > = != like glob is and or * / % << >> + & |)
             (if (= 2 (length args))
                 (format "%s %s %s"
                         (recur 0)
                         (if (eq op '%) '%% (upcase (symbol-name op)))
                         (recur 1))
               (error "Wrong number of operands for %s" op)))
            ;; Unary
            ((not)
             (if (= 1 (length args))
                 (format "%s %s" (upcase (symbol-name op)) (recur 0))
               (error "Wrong number of operands for %s" op)))
            ;; Unary/Binary
            ((-)
             (cl-ecase (length args)
               (1 (format "-(%s)" (recur 0)))
               (2 (format "%s - %s" (recur 0) (recur 1)))))
            ;; IN special case
            ((in)
             (cl-case (length args)
               (1 (error "Wrong number of operands for %s" op))
               (2 (format "%s IN %s" (recur 0) (var (nth 1 args) :vector)))
               (otherwise
                (format "%s IN %s" (recur 0) (subsql (cdr args))))))))))))

(defun emacsql--idents (idents)
  "Read in a vector of IDENTS identifiers, or just an single identifier."
  (emacsql-with-vars ""
    (cl-etypecase idents
      (symbol (var idents :identifier))
      (vector (mapconcat (lambda (e) (expr e)) idents ", ")))))

(defun emacsql-init-font-lock ()
  "Add font-lock highlighting for `emacsql-defexpander'."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(emacsql-defexpander\\)\\_>"
      (1 'font-lock-keyword-face)))))

;; SQL Expansion Functions:

(emacsql-defexpander :select (arg)
  "Expands to the SELECT keyword."
  (emacsql-with-vars "SELECT "
    (if (eq '* arg)
        "*"
      (idents arg))))

(emacsql-defexpander :from (sources)
  "Expands to the FROM keyword."
  (emacsql-with-vars "FROM "
    (cl-etypecase sources
      (symbol (var sources :identifier))
      (list (if (eq :select (car sources))
                (subsql sources)
              (cl-destructuring-bind (table alias) sources
                (concat (var table :identifier) " " (var alias :identifier)))))
      (vector (mapconcat (lambda (source)
                           (cl-etypecase source
                             (symbol (var source :identifier))
                             (list
                              (cl-destructuring-bind (table alias) source
                                (concat (if (symbolp table)
                                            (var table :identifier)
                                          (subsql table))
                                        " " (var alias :identifier))))))
                         sources ", ")))))

(emacsql-defexpander :replace ()
  (list "REPLACE"))

(emacsql-defexpander :insert ()
  (list "INSERT"))

(emacsql-defexpander :into (table)
  "Expands to the INTO keywords."
  (emacsql-with-vars "INTO "
    (cl-typecase table
      (symbol (var table :identifier))
      (list (cl-destructuring-bind (name columns) table
              (format "%s (%s)" (var name :identifier)
                      (idents columns)))))))

(emacsql-defexpander :where (expr)
  (emacsql-with-vars "WHERE "
    (expr expr)))

(emacsql-defexpander :group-by (expr)
  (emacsql-with-vars "GROUP BY "
    (expr expr)))

(emacsql-defexpander :order-by (columns)
  (emacsql-with-vars "ORDER BY "
    (cl-flet ((order (k) (cl-ecase k (:asc " ASC") (:desc " DESC"))))
      (if (not (vectorp columns))
          (expr columns)
        (cl-loop for column across columns collect
                 (cl-etypecase column
                   (list (let ((kpos (cl-position-if #'keywordp column)))
                           (if kpos
                               (concat (expr (nth (- 1 kpos) column))
                                       (order (nth kpos column)))
                             (expr column))))
                   (symbol (var column :identifier)))
                 into parts
                 finally (cl-return (mapconcat #'identity parts ", ")))))))

(emacsql-defexpander :limit (limits)
  (emacsql-with-vars "LIMIT "
    (if (vectorp limits)
        (mapconcat #'expr limits ", ")
      (expr limits))))

(emacsql-defexpander :create-table (table schema)
  (emacsql-with-vars "CREATE "
    (let (temporary if-not-exists name)
      (dolist (item (if (listp table) table (list table)))
        (cl-case item
          (:if-not-exists (setf if-not-exists "IF NOT EXISTS"))
          (:temporary (setf temporary "TEMPORARY"))
          (otherwise (setf name (var item :identifier)))))
      (let* ((items (list temporary "TABLE" if-not-exists name))
             (spec (cl-remove-if-not #'identity items)))
        (format "%s %s" (mapconcat #'identity spec " ")
                (cond ((symbolp schema)
                       (format "(%s)" (var schema :schema)))
                      ((eq :select (elt schema 0))
                       (concat "AS " (subsql schema)))
                      ((let ((compiled (emacsql--schema-to-string schema)))
                         (format "(%s)" (combine compiled))))))))))

(emacsql-defexpander :drop-table (table)
  (emacsql-with-vars "DROP TABLE "
    (var table :identifier)))

(emacsql-defexpander :delete ()
  (list "DELETE"))

(emacsql-defexpander :values (values)
  (emacsql-with-vars "VALUES "
    (combine (emacsql--vector values))))

(emacsql-defexpander :update (table)
  (emacsql-with-vars "UPDATE "
    (var table :identifier)))

(emacsql-defexpander :set (set)
  (emacsql-with-vars "SET "
    (cl-etypecase set
      (vector (idents set))
      (list (expr set)))))

(emacsql-defexpander :union ()
  (list "UNION"))

(emacsql-defexpander :union-all ()
  (list "UNION ALL"))

(emacsql-defexpander :intersect ()
  (list "INTERSECT"))

(emacsql-defexpander :except ()
  (list "EXCEPT"))

(emacsql-defexpander :pragma (expr)
  (emacsql-with-vars "PRAGMA "
    (expr expr)))

(provide 'emacsql)

;;; emacsql.el ends here
