;;; emacsql-tests.el --- tests for emacsql

;;; Code:

(require 'ert)
(require 'emacsql)

(ert-deftest emacsql-escape-identifier ()
  (should (string= (emacsql-escape-identifier "foo") "foo"))
  (should (string= (emacsql-escape-identifier 'foo) "foo"))
  (should (string= (emacsql-escape-identifier :foo) "':foo'"))
  (should (string= (emacsql-escape-identifier "a b") "'a b'"))
  (should (string= (emacsql-escape-identifier '$foo) "'$foo'"))
  (should (string= (emacsql-escape-identifier "foo$") "foo$"))
  (should (string= (emacsql-escape-identifier "they're") "'they''re'")))

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

(ert-deftest emacsql-expand ()
  (emacsql-tests-query [:select [$1 name] :from $2] '(id people)
                       "SELECT id, name FROM people;")
  (emacsql-tests-query [:select * :from employees] ()
                       "SELECT * FROM employees;")
  (emacsql-tests-query [:select * :from employees :where (< salary 50000)] ()
                       "SELECT * FROM employees WHERE salary < 50000;")
  (emacsql-tests-query [:create-table foo [a b c]] ()
                       "CREATE TABLE foo (a, b, c);")
  (emacsql-tests-query [:drop-table $1] '(foo)
                       "DROP TABLE foo;")
  (emacsql-tests-query [:update people :set (= id $1)] '(10)
                       "UPDATE people SET id = 10;")
  (emacsql-tests-query [:select * :from people :where (in name $1)] '([FOO BAR])
                       "SELECT * FROM people WHERE name IN ('FOO', 'BAR');")
  (emacsql-tests-query [:insert :into foo :values [nil $1]] '(10.1)
                       "INSERT INTO foo VALUES (NULL, 10.1);"))

(ert-deftest emacsql-system ()
  (emacsql-with-connection (db nil)
    (emacsql db [:create-table foo [x]])
    (should-error (emacsql db [:create-table foo [x]]))
    (emacsql db [:insert :into foo :values ([1] [2] [3])])
    (should (equal (emacsql db [:select * :from foo])
                   '((1) (2) (3))))))

(provide 'emacsql-tests)

;;; emacsql-tests.el ends here
