;;; emacsql-tests.el --- tests for emacsql

;;; Code:

(require 'ert)
(require 'emacsql)

(ert-deftest emacsql-escape-identifier ()
  (should (string= (emacsql-escape-identifier "foo") "foo"))
  (should (string= (emacsql-escape-identifier 'foo) "foo"))
  (should (string= (emacsql-escape-identifier :foo) "foo"))
  (should-error (emacsql-escape-identifier "a b"))
  (should-error (emacsql-escape-identifier '$foo))
  (should-error (emacsql-escape-identifier 10))
  (should (string= (emacsql-escape-identifier 'foo$) "foo$"))
  (should (string= (emacsql-escape-identifier "foo:bar") "foo.bar")))

(ert-deftest emacsql-escape-value ()
  (should (string= (emacsql-escape-value 'foo) "'foo'"))
  (should (string= (emacsql-escape-value "foo") "'\"foo\"'"))
  (should (string= (emacsql-escape-value :foo) "':foo'"))
  (should (string= (emacsql-escape-value [1 2 3]) "'[1 2 3]'"))
  (should (string= (emacsql-escape-value '(a b c)) "'(a b c)'"))
  (should (string= (emacsql-escape-value nil) "NULL")))

(ert-deftest emacsql-escape-vector ()
  (should (string= (emacsql-escape-vector [1 2 3]) "(1, 2, 3)"))
  (should (string= (emacsql-escape-vector '([1 2 3])) "(1, 2, 3)"))
  (should (string= (emacsql-escape-vector '([1 2 3] [4 5 6]))
                   "(1, 2, 3), (4, 5, 6)")))

(ert-deftest emacsql-schema ()
  (should (string= (emacsql--schema-to-string [a]) "a"))
  (should (string= (emacsql--schema-to-string [a b c]) "a, b, c"))
  (should (string= (emacsql--schema-to-string [a (b)]) "a, b"))
  (should (string= (emacsql--schema-to-string [a (b float)])
                   "a, b REAL"))
  (should (string= (emacsql--schema-to-string [a (b :primary float :unique)])
                   "a, b REAL PRIMARY KEY UNIQUE"))
  (should (string= (emacsql--schema-to-string [(a integer) (b float)])
                   "a INTEGER, b REAL")))

(ert-deftest emacsql-var ()
  (should (eq (emacsql-var 'a) nil))
  (should (eq (emacsql-var 0) nil))
  (should (eq (emacsql-var "") nil))
  (should (eq (emacsql-var '$) 0))
  (should (eq (emacsql-var '$1) 0))
  (should (eq (emacsql-var '$5) 4))
  (should (eq (emacsql-var '$10) 9)))

(defun emacsql-tests-query (query args result)
  "Check that QUERY outputs RESULT for ARGS."
  (should (string= (apply #'emacsql-format (emacsql-expand query) args)
                   result)))

(defmacro emacsql-tests-with-queries (&rest queries)
  "Thread `emacsql-tests-query' through QUERIES."
  (declare (indent 0))
  (cons 'progn (mapcar (lambda (q) (cons 'emacsql-tests-query q)) queries)))

(ert-deftest emacsql-select ()
  (emacsql-tests-with-queries
    ([:select [$1 name] :from $2] '(id people)
     "SELECT id, name FROM people;")
    ([:select * :from employees] '()
     "SELECT * FROM employees;")
    ([:select * :from employees :where (< salary 50000)] '()
     "SELECT * FROM employees WHERE salary < 50000;")
    ([:select * :from people :where (in name $1)] '([FOO BAR])
     "SELECT * FROM people WHERE name IN ('FOO', 'BAR');")))

(ert-deftest emacsql-create-table ()
  (emacsql-tests-with-queries
    ([:create-table foo [a b c]] ()
     "CREATE TABLE foo (a, b, c);")
    ([:create-table (:temporary :if-not-exists x) [y]] '()
     "CREATE TEMPORARY TABLE IF NOT EXISTS x (y);")
    ([:create-table foo [(a :default 10)]] '()
     "CREATE TABLE foo (a DEFAULT 10);")
    ([:create-table foo [(a :primary :non-nil) b]] '()
     "CREATE TABLE foo (a PRIMARY KEY NOT NULL, b);")
    ([:drop-table $1] '(foo)
     "DROP TABLE foo;")))

(ert-deftest emacsql-update ()
  (emacsql-tests-with-queries
    ([:update people :set (= id $1)] '(10)
     "UPDATE people SET id = 10;")))

(ert-deftest emacsql-insert ()
  (emacsql-tests-with-queries
    ([:insert :into foo :values [nil $1]] '(10.1)
     "INSERT INTO foo VALUES (NULL, 10.1);")
    ([:insert :into (foo [a b]) :values $1] '([1 2])
     "INSERT INTO foo (a, b) VALUES (1, 2);")
    ([:replace :into $1 :values $2] '(bar ([1 2] [3 4]))
     "REPLACE INTO bar VALUES (1, 2), (3, 4);")))

(ert-deftest emacsql-order-by ()
  (emacsql-tests-with-queries
    ([:order-by foo] '()
     "ORDER BY foo;")
    ([:order-by [$1]] '(bar)
     "ORDER BY bar;")
    ([:order-by (- foo)] '()
     "ORDER BY -(foo);")
    ([:order-by [(a :asc) ((/ b 2) :desc)]] '()
     "ORDER BY a ASC, b / 2 DESC;")))

(ert-deftest emacsql-limit ()
  (emacsql-tests-with-queries
    ([:limit 10] '()
     "LIMIT 10;")
    ([:limit $1] '(11)
     "LIMIT 11;")
    ([:limit [12]] '()
     "LIMIT 12;")
    ([:limit [2 10]] '()
     "LIMIT 2, 10;")
    ([:limit [$1 $2]] '(4 30)
     "LIMIT 4, 30;")))

(ert-deftest emacsql-system ()
  (should-not (emacsql-sqlite3-unavailable-p))
  (emacsql-with-connection (db nil)
    (emacsql db [:create-table foo [x]])
    (should-error (emacsql db [:create-table foo [x]]))
    (emacsql db [:insert :into foo :values ([1] [2] [3])])
    (should (equal (emacsql db [:select * :from foo])
                   '((1) (2) (3))))))

(provide 'emacsql-tests)

;;; emacsql-tests.el ends here
