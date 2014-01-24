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

(defclass emacsql-connection ()
  ((process :type process
            :initarg :process
            :accessor emacsql-process)
   (log-buffer :type (or null buffer)
               :initarg :log-buffer
               :accessor emacsql-log-buffer
               :documentation "Output log (debug)."))
  (:documentation "A connection to a SQL database.")
  :abstract t)

(defgeneric emacsql (connection sql &rest args)
  "Send SQL s-expression to CONNECTION and return the results.")

(defgeneric emacsql-close (connection)
  "Close CONNECTION and free all resources.")

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

;; Standard Emacsql errors:

(defmacro emacsql-deferror (symbol parents message)
  "Defines a new error symbol  for Emacsql."
  (declare (indent 2))
  (let ((conditions (cl-remove-duplicates
                     (append parents (list symbol 'emacsql-error 'error)))))
    `(prog1 ',symbol
       (setf (get ',symbol 'error-conditions) ',conditions
             (get ',symbol 'error-message) ,message))))

(emacsql-deferror emacsql-error () ;; parent condition for all others
  "Emacsql had an unhandled condition")

(emacsql-deferror emacsql-syntax () "Invalid SQL statement")
(emacsql-deferror emacsql-table () "Table error")
(emacsql-deferror emacsql-lock () "Database locked")
(emacsql-deferror emacsql-transaction () "Invalid transaction")
(emacsql-deferror emacsql-fatal () "Fatal error")
(emacsql-deferror emacsql-access () "Database access error")

(defun emacsql-error (format &rest args)
  "Like `error', but signal an emacsql-syntax condition."
  (signal 'emacsql-syntax (list (apply #'format format args))))

;;; Sending and receiving:

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

(provide 'emacsql) ; end of generic function declarations

;; Automatic connection cleanup:

(defvar emacsql-connections ()
  "Collection of all known emacsql connections.
This collection exists for cleanup purposes.")

(defvar emacsql-reap-timer nil
  "Timer used to check for dead emacsql connections.")

(defun emacsql-register (connection)
  "Add CONNECTION to the global connection list."
  (emacsql-start-reap-timer)
  (push (cons (copy-sequence connection) (emacsql--ref connection))
        emacsql-connections))

(defun emacsql--ref (thing)
  "Create a weak reference to THING."
  (let ((ref (make-hash-table :test 'eq :size 1 :weakness 'value)))
    (prog1 ref
      (setf (gethash t ref) thing))))

(defun emacsql--deref (ref)
  "Retrieve value from REF."
  (gethash t ref))

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

;;; Escaping:

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
      (emacsql-error "Invalid Emacsql identifier: %S" identifier))
    (if (string-match-p ":" string)
        (replace-regexp-in-string ":" "." string)
      string)))

(defun emacsql-escape-value (value)
  "Escape VALUE for sending to SQLite."
  (let ((print-escape-newlines t))
    (cond ((null value) "NULL")
          ((numberp value) (prin1-to-string value))
          ((emacsql-quote (prin1-to-string value))))))

(defun emacsql-escape-vector (vector)
  "Encode VECTOR into a SQL vector scalar."
  (cl-typecase vector
    (list   (mapconcat #'emacsql-escape-vector vector ", "))
    (vector (concat "(" (mapconcat #'emacsql-escape-value vector ", ") ")"))
    (otherwise (emacsql-error "Invalid vector %S" vector))))

;; S-expression SQL compilation:

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

(defun emacsql-sql-p (thing)
  "Return non-nil if THING looks like a :select."
  (and (sequencep thing)
       (or (not (null (assoc (elt thing 0) emacsql-expanders)))
           (emacsql-sql-p (elt thing 0)))))

(defun emacsql-get-expander (keyword)
  "Return the expander with arity for KEYWORD."
  (if (emacsql-sql-p keyword)
      (list 0 (lambda () (emacsql-expand keyword :subsql-p)))
    (cdr (assoc keyword emacsql-expanders))))

(defun emacsql-expand (sql &optional subsql-p)
  "Expand SQL into a SQL-consumable string, with variables."
  (let* ((cache emacsql-expander-cache)
         (cached (and cache (gethash sql cache))))
    (or cached
        (cl-loop with items = (cl-coerce sql 'list)
                 while (not (null items))
                 for keyword = (pop items)
                 for (arity expander) = (emacsql-get-expander keyword)
                 when expander
                 collect (apply expander (cl-subseq items 0 arity)) into parts
                 else do (emacsql-error "Unrecognized keyword %s" keyword)
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
      (emacsql-error "Wrong number of arguments for SQL template."))
    (apply #'format format
           (cl-loop for (i . kind) in vars collect
                    (let ((thing (nth i args)))
                      (cl-case kind
                        (:identifier (emacsql-escape-identifier thing))
                        (:value (emacsql-escape-value thing))
                        (:vector (emacsql-escape-vector thing))
                        (:schema (car (emacsql--schema-to-string thing)))
                        (:auto (if (symbolp thing)
                                   (emacsql-escape-identifier thing)
                                 (emacsql-escape-value thing)))
                        (otherwise
                         (emacsql-error "Invalid var type %S" kind))))))))

(defun emacsql-compile (sql &rest args)
  "Compile s-expression SQL expression into a string."
  (apply #'emacsql-format (emacsql-expand sql) args))

(defun emacsql-var (var)
  "Return the index number of VAR, or nil if VAR is not a variable.
A variable is a symbol that looks like $1, $2, $3, etc. A $ means
$1. These are escaped with a double $$, in which case the proper
symbol is returned."
  (when (symbolp var)
    (let ((name (symbol-name var)))
      (cond
       ((string-match-p "^\\$[0-9]+" name) (1- (read (substring name 1))))
       ((string-match-p "^\\$$" name) 0)
       ((string-match-p "^\\$\\$[0-9]+" name) (intern (substring name 1)))))))

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
  (let ((var (emacsql-var thing)))
    (when (and var (symbolp var)) (setf thing var))
    (if (numberp var)
        (prog1 "%s" (push (cons var kind) emacsql--vars))
      (cl-case kind
        ((:identifier :value :vector) (emacsql-escape-format thing kind))
        (:auto (emacsql-escape-format
                thing (if (symbolp thing) :identifier :value)))
        (otherwise (emacsql-error "Invalid var type: %S" kind))))))

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
            (:autoincrement (push "AUTOINCREMENT" output))
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
                 (emacsql-error "Unknown schema contraint %s" next)
               (emacsql-error "Invalid type %s: %s" next
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
                 (cl-case key
                   (:on-update "ON UPDATE")
                   (:on-delete "ON DELETE")
                   (otherwise (emacsql-error "Invalid case: %S" key)))
                 collect
                 (cl-case value
                   (:restrict "RESTRICT")
                   (:set-nil "SET NULL")
                   (:set-default "SET DEFAULT")
                   (:cascade "CASCADE")
                   (otherwise (emacsql-error "Invalid action: %S" key)))))
       " "))))

(defun emacsql--schema-to-string (schema)
  (cl-typecase schema
    (vector (emacsql--columns-to-string schema))
    (list
     (emacsql-with-vars ""
       (mapconcat
        #'identity
        (cons
         (combine (emacsql--columns-to-string (pop schema)))
         (cl-loop for (key value) on schema by #'cddr collect
                  (cl-case key
                    (:primary (format "PRIMARY KEY (%s)" (idents value)))
                    (:unique (format "UNIQUE (%s)" (idents value)))
                    (:check (format "CHECK (%s)" (expr value)))
                    (:foreign (combine (emacsql--foreign-key value)))
                    (otherwise
                     (emacsql-error "Invalid table constraint: %S" key)))))
        ", ")))
    (otherwise (emacsql-error "Invalid schema: %S" schema))))

(defun emacsql--vector (vector)
  "Expand VECTOR, making variables as needed."
  (emacsql-with-vars ""
    (cl-typecase vector
      (symbol
       (var vector :vector))
      (list
       (mapconcat (lambda (v) (combine (emacsql--vector v))) vector ", "))
      (vector
       (format "(%s)" (mapconcat (lambda (x) (var x :value)) vector ", ")))
      (otherwise (emacsql-error "Invalid vector: %S" vector)))))

(defun emacsql--expr (expr)
  "Expand EXPR recursively."
  (emacsql-with-vars ""
    (cond
     ((emacsql-sql-p expr) (subsql expr))
     ((atom expr) (var expr :auto))
     ((cl-destructuring-bind (op . args) expr
         (cl-flet ((recur (n) (combine (emacsql--expr (nth n args))))
                   (nops (op)
                     (emacsql-error "Wrong number of operands for %s" op)))
           (cl-case op
             ;; Trinary/binary
             ((<= >=)
              (cl-case (length args)
                (2 (format "%s %s %s" (recur 0) op (recur 1)))
                (3 (format "%s BETWEEN %s AND %s"
                           (recur 1)
                           (recur (if (eq op '>=) 2 0))
                           (recur (if (eq op '>=) 0 2))))
                (otherwise (nops op))))
             ;; Binary
             ((< > = != like glob is * / % << >> + & | as)
              (if (= 2 (length args))
                  (format "%s %s %s"
                          (recur 0)
                          (if (eq op '%) '%% (upcase (symbol-name op)))
                          (recur 1))
                (nops op)))
             ;; Unary
             ((not)
              (if (= 1 (length args))
                  (format "%s %s" (upcase (symbol-name op)) (recur 0))
                (nops op)))
             ;; Unary/Binary
             ((-)
              (cl-case (length args)
                (1 (format "-(%s)" (recur 0)))
                (2 (format "%s - %s" (recur 0) (recur 1)))
                (otherwise (nops op))))
             ;; Variadic
             ((and or)
              (cl-case (length args)
                (0 (if (eq op 'and) "1" "0"))
                (1 (recur 0))
                (otherwise
                 (mapconcat
                  #'recur (cl-loop for i from 0 below (length args) collect i)
                  (format " %s " (upcase (symbol-name op)))))))
             ;; quote special case
             ((quote)
              (cl-case (length args)
                (1 (var (nth 0 args) :value))
                (otherwise (nops op))))
             ;; IN special case
             ((in)
              (cl-case (length args)
                (1 (emacsql-error "Wrong number of operands for %s" op))
                (2 (format "%s IN %s" (recur 0) (var (nth 1 args) :vector)))
                (otherwise
                 (format "%s IN %s" (recur 0) (subsql (cdr args))))))
             (otherwise (emacsql-error "Unknown operator: %S" op)))))))))

(defun emacsql--idents (idents)
  "Read in a vector of IDENTS identifiers, or just an single identifier."
  (emacsql-with-vars ""
    (cl-typecase idents
      (symbol (var idents :identifier))
      (list (expr idents))
      (vector (mapconcat (lambda (e) (expr e)) idents ", "))
      (otherwise (emacsql-error "Invalid syntax: %S" idents)))))

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
    (idents sources)))

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

(emacsql-defexpander :having (expr)
  (emacsql-with-vars "HAVING "
    (expr expr)))

(emacsql-defexpander :group-by (expr)
  (emacsql-with-vars "GROUP BY "
    (expr expr)))

(emacsql-defexpander :order-by (columns)
  (emacsql-with-vars "ORDER BY "
    (cl-flet ((order (k) (cl-case k
                           (:asc " ASC")
                           (:desc " DESC")
                           (otherwise (emacsql-error "Invalid order: %S" k)))))
      (if (not (vectorp columns))
          (expr columns)
        (cl-loop for column across columns collect
                 (cl-typecase column
                   (list (let ((kpos (cl-position-if #'keywordp column)))
                           (if kpos
                               (concat (expr (nth (- 1 kpos) column))
                                       (order (nth kpos column)))
                             (expr column))))
                   (symbol (var column :identifier))
                   (otherwise (emacsql-error "Invalid order spec: %S" column)))
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
    (cl-typecase set
      (vector (idents set))
      (list (expr set))
      (otherwise (emacsql-error "Invalid SET expression: %S" set)))))

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

(emacsql-defexpander :begin (kind)
  (emacsql-with-vars "BEGIN "
    (cl-case kind
      (:transaction "TRANSACTION")
      (:deferred    "DEFERRED")
      (:immediate   "IMMEDIATE")
      (:exclusive   "EXCLUSIVE")
      (otherwise (emacsql-error "Unknown transaction type: %S" kind)))))

(emacsql-defexpander :commit ()
  (list "COMMIT"))

(emacsql-defexpander :rollback ()
  (list "ROLLBACK"))

(emacsql-defexpander :vacuum ()
  (list "VACUUM"))

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
(defun emacsql-show-last-sql ()
  "Display the compiled SQL of the s-expression SQL expression before point."
  (interactive)
  (emacsql-show-sql (emacsql-flatten-sql (preceding-sexp))))

;;; emacsql.el ends here
