;;; emacsql-compile.el --- s-expression SQL compiler -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

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
(emacsql-deferror emacsql-timeout () "Query timeout error")

(defun emacsql-error (format &rest args)
  "Like `error', but signal an emacsql-syntax condition."
  (signal 'emacsql-syntax (list (apply #'format format args))))

;; Escaping functions:

(defun emacsql-quote (string)
  "Quote STRING for use in a SQL expression."
  (format "'%s'" (replace-regexp-in-string "'" "''" string)))

(defun emacsql-escape-identifier (identifier)
  "Escape an identifier, always with quotes when FORCE is non-nil."
  (let ((string (cl-typecase identifier
                  (string identifier)
                  (keyword (substring (symbol-name identifier) 1))
                  (otherwise (format "%S" identifier))))
        (forbidden "[]\000-\040!\"#%&'()*+,./;<=>?@[\\^`{|}~\177]"))
    (when (or (null identifier)
              (string-match-p forbidden string)
              (string-match-p "^[0-9$]" string))
      (emacsql-error "Invalid Emacsql identifier: %S" identifier))
    (setf string (replace-regexp-in-string ":" "." string))
    (setf string (replace-regexp-in-string "-" "_" string))
    string))

(defun emacsql-escape-value (value)
  "Escape VALUE for sending to SQLite."
  (let ((print-escape-newlines t))
    (cond ((null value) "NULL")
          ((numberp value) (prin1-to-string value))
          ((emacsql-quote (prin1-to-string value))))))

(defun emacsql-escape-vector (vector)
  "Encode VECTOR into a SQL vector scalar."
  (cl-typecase vector
    (null   (emacsql-error "Empty SQL vector expression."))
    (list   (mapconcat #'emacsql-escape-vector vector ", "))
    (vector (concat "(" (mapconcat #'emacsql-escape-value vector ", ") ")"))
    (otherwise (emacsql-error "Invalid vector %S" vector))))

;; Statement compilers:

(defvar emacsql-expanders ()
  "Alist of all expansion functions.")

(defvar emacsql-expander-cache (make-hash-table :test 'equal)
  "Cache used to memoize `emacsql-expand'.")

(defvar emacsql-type-map
  '((integer "INTEGER")
    (float "REAL")
    (object "TEXT")
    (nil "NONE"))
  "An alist mapping Emacsql types to SQL types.")

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
         (key (cons emacsql-type-map sql))
         (cached (and cache (gethash key cache))))
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
                                  (setf (gethash key cache) (cons string vars))
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
                        (:auto (if (and thing (symbolp thing))
                                   (emacsql-escape-identifier thing)
                                 (emacsql-escape-value thing)))
                        (otherwise
                         (emacsql-error "Invalid var type %S" kind))))))))

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
        (prog1 "%s"
          (setf emacsql--vars (nconc emacsql--vars (list (cons var kind)))))
      (cl-case kind
        ((:identifier :value :vector) (emacsql-escape-format thing kind))
        (:auto (emacsql-escape-format
                thing (if (and thing (symbolp thing)) :identifier :value)))
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
    (when (symbolp column)
      (setf column (list column)))
    (let ((name (var (pop column) :identifier))
          (output ())
          (type (cadr (assoc nil emacsql-type-map))))
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
            (:references
             (push (combine (emacsql--references (pop column))) output))
            ((integer float object)
             (setf type (cadr (assoc next emacsql-type-map))))
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
             collect (combine (emacsql--column-to-string column)) into parts
             finally (cl-return (mapconcat #'identity parts ", ")))))

(defun emacsql--references (spec)
  (emacsql-with-vars "REFERENCES "
    (cl-destructuring-bind (table parent . actions) (cl-coerce spec 'list)
      (mapconcat
       #'identity
       (cons
        (format "%s (%s)" (var table :identifier) (idents parent))
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

(defun emacsql--foreign-key (spec)
  (emacsql-with-vars "FOREIGN KEY "
    (cl-destructuring-bind (child . references) (cl-coerce spec 'list)
      (format "(%s) %s" (idents child)
              (combine (emacsql--references references))))))

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
                    (:references (combine (emacsql--foreign-key value)))
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
             ;; funcall special case
             ((funcall)
              (cl-case (length args)
                (2 (format "%s(%s)" (var (nth 0 args) :identifier) (recur 1)))
                (otherwise
                 (emacsql-error "Wrong number of operands for %s" op))))
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
    (cond ((eq '* arg)
           "*")
          ((listp arg)
           (cl-case (length arg)
             (1 (idents (car arg)))
             (2 (cl-case (car arg)
                  (:distinct (concat "DISTINCT " (idents (cadr arg))))
                  (:all (concat "ALL " (idents (cadr arg))))
                  (otherwise (emacsql-error "Invalid SELECT: %S" (car arg)))))
             (otherwise (emacsql-error "Invalid SELECT idents: %S" arg))))
          ((idents arg)))))

(emacsql-defexpander :from (sources)
  "Expands to the FROM keyword."
  (emacsql-with-vars "FROM "
    (idents sources)))

(emacsql-defexpander :join (source)
  (emacsql-with-vars "JOIN "
    (idents source)))

(emacsql-defexpander :natural ()
  (list "NATURAL"))

(emacsql-defexpander :outer ()
  (list "OUTER"))

(emacsql-defexpander :inner ()
  (list "INNER"))

(emacsql-defexpander :cross ()
  (list "CROSS"))

(emacsql-defexpander :left ()
  (list "LEFT"))

(emacsql-defexpander :right ()
  (list "RIGHT"))

(emacsql-defexpander :full ()
  (list "FULL"))

(emacsql-defexpander :on (expr)
  (emacsql-with-vars "ON "
    (expr expr)))

(emacsql-defexpander :using (columns)
  (emacsql-with-vars "USING "
    (format "(%s)" (idents columns))))

(emacsql-defexpander :insert ()
  (list "INSERT"))

(emacsql-defexpander :replace ()
  (list "REPLACE"))

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

(emacsql-defexpander :alter-table (table)
  (emacsql-with-vars "ALTER TABLE "
    (var table :identifier)))

(emacsql-defexpander :add-column (column)
  (emacsql-with-vars "ADD COLUMN "
    (cl-typecase column
      (symbol (var column :identifier))
      (list (combine (emacsql--column-to-string column)))
      (otherwise (emacsql-error "Only one column allowed here: %S" column)))))

(emacsql-defexpander :rename-to (new-name)
  (emacsql-with-vars "RENAME TO "
    (var new-name :identifier)))

(emacsql-defexpander :vacuum ()
  (list "VACUUM"))

(provide 'emacsql-compiler)

;;; emacsql-compile.el ends here
