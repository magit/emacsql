;;; emacsql-tests.el --- Tests for emacsql  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'ert)
(require 'emacsql)

(ert-deftest emacsql-escape-identifier ()
  (should-error (emacsql-escape-identifier "foo"))
  (should (string= (emacsql-escape-identifier 'foo) "foo"))
  (should (string= (emacsql-escape-identifier 'a\ b) "\"a\\ b\""))
  (should (string= (emacsql-escape-identifier '$foo) "\"$foo\""))
  (emacsql-register-reserved '(SELECT))
  (should (string= (emacsql-escape-identifier 'select) "\"select\""))
  (should-error (emacsql-escape-identifier 10))
  (should-error (emacsql-escape-identifier nil))
  (should (string= (emacsql-escape-identifier 'person-id) "person_id"))
  (should (string= (emacsql-escape-identifier 'people:person-id)
                   "people.person_id"))
  (should (string= (emacsql-escape-identifier 'foo$) "foo$"))
  (should (string= (emacsql-escape-identifier 'foo:bar) "foo.bar")))

(ert-deftest emacsql-escape-scalar ()
  (should (string= (emacsql-escape-scalar 'foo) "'foo'"))
  (should (string= (emacsql-escape-scalar "foo") "'\"foo\"'"))
  (should (string= (emacsql-escape-scalar :foo) "':foo'"))
  (should (string= (emacsql-escape-scalar [1 2 3]) "'[1 2 3]'"))
  (should (string= (emacsql-escape-scalar '(a b c)) "'(a b c)'"))
  (should (string= (emacsql-escape-scalar nil) "NULL")))

(ert-deftest emacsql-escape-vector ()
  (should (string= (emacsql-escape-vector [1 2 3]) "(1, 2, 3)"))
  (should (string= (emacsql-escape-vector '([1 2 3])) "(1, 2, 3)"))
  (should (string= (emacsql-escape-vector '([1 2 3] [4 5 6]))
                   "(1, 2, 3), (4, 5, 6)")))

(ert-deftest emacsql-escape-raw ()
  (should (string= (emacsql-escape-raw "/var/emacsql") "'/var/emacsql'"))
  (should (string= (emacsql-escape-raw "a b c") "'a b c'"))
  (should (string= (emacsql-escape-raw "a 'b' c") "'a ''b'' c'"))
  (should (string= (emacsql-escape-raw nil) "NULL")))

(ert-deftest emacsql-schema ()
  (should (string= (emacsql-prepare-schema [a]) "a &NONE"))
  (should (string= (emacsql-prepare-schema [a b c])
                   "a &NONE, b &NONE, c &NONE"))
  (should (string= (emacsql-prepare-schema [a (b)])
                   "a &NONE, b &NONE"))
  (should (string= (emacsql-prepare-schema [a (b float)])
                   "a &NONE, b &REAL"))
  (should (string= (emacsql-prepare-schema
                    [a (b float :primary-key :unique)])
                   "a &NONE, b &REAL PRIMARY KEY UNIQUE"))
  (should (string= (emacsql-prepare-schema [(a integer) (b float)])
                   "a &INTEGER, b &REAL")))

(ert-deftest emacsql-param ()
  (should (equal (emacsql-param 'a) nil))
  (should (equal (emacsql-param 0) nil))
  (should (equal (emacsql-param "") nil))
  (should (equal (emacsql-param '$) nil))
  (should (equal (emacsql-param '$1) nil))
  (should (equal (emacsql-param '$s5) '(4 . :scalar)))
  (should (equal (emacsql-param '$v10) '(9 . :vector)))
  (should (equal (emacsql-param '$r2) '(1 . :raw)))
  (should (equal (emacsql-param '$a) nil))
  (should (equal (emacsql-param '$i10) '(9 . :identifier))))

(defun emacsql-tests-query (query args result)
  "Check that QUERY outputs RESULT for ARGS."
  (should (string= (apply #'emacsql-compile nil query args)
                   result)))

(defmacro emacsql-tests-with-queries (&rest queries)
  "Thread `emacsql-tests-query' through QUERIES."
  (declare (indent 0))
  (cons 'progn (mapcar (lambda (q) (cons 'emacsql-tests-query q)) queries)))

(ert-deftest emacsql-select ()
  (emacsql-tests-with-queries
    ([:select [$i1 name] :from $i2] '(id people)
     "SELECT id, name FROM people;")
    ([:select * :from employees] '()
     "SELECT * FROM employees;")
    ([:select * :from employees :where (< salary 50000)] '()
     "SELECT * FROM employees WHERE salary < 50000;")
    ([:select * :from people :where (in name $v1)] '([FOO BAR])
     "SELECT * FROM people WHERE name IN ('FOO', 'BAR');")
    ;; Sub queries
    ([:select name :from [:select * :from $i1]] '(people)
     "SELECT name FROM (SELECT * FROM people);")
    ([:select name :from [people (as accounts a)]] '()
     "SELECT name FROM people, accounts AS a;")
    ([:select p:name :from [(as [:select * :from people] p)]] '()
     "SELECT p.name FROM (SELECT * FROM people) AS p;")))

(ert-deftest emacsql-attach ()
  (emacsql-tests-with-queries
    ([:attach $r1 :as $i2] '("/var/foo.db" foo)
     "ATTACH '/var/foo.db' AS foo;")
    ([:detach $i1] '(foo)
     "DETACH foo;")))

(ert-deftest emacsql-create-table ()
  (emacsql-tests-with-queries
    ([:create-table foo ([a b c])] ()
     "CREATE TABLE foo (a &NONE, b &NONE, c &NONE);")
    ([:create-temporary-table :if-not-exists x ([y])] '()
     "CREATE TEMPORARY TABLE IF NOT EXISTS x (y &NONE);")
    ([:create-table foo ([(a :default 10)])] '()
     "CREATE TABLE foo (a &NONE DEFAULT 10);")
    ([:create-table foo ([(a :primary-key :not-null) b])] '()
     "CREATE TABLE foo (a &NONE PRIMARY KEY NOT NULL, b &NONE);")
    ([:create-table foo ([a (b :check (< b 10))])] '()
     "CREATE TABLE foo (a &NONE, b &NONE CHECK (b < 10));")
    ([:create-table foo $S1] '([a b (c :primary-key)])
     "CREATE TABLE foo (a &NONE, b &NONE, c &NONE PRIMARY KEY);")
    ([:create-table foo ([a b (c :default "FOO")])] '()
     "CREATE TABLE foo (a &NONE, b &NONE, c &NONE DEFAULT '\"FOO\"');")
    ;; From select
    ([:create-table $i1 :as [:select name :from $i2]] '(names people)
     "CREATE TABLE names AS (SELECT name FROM people);")
    ;; Table constraints
    ([:create-table foo ([a b c] (:primary-key [a c]))] '()
     "CREATE TABLE foo (a &NONE, b &NONE, c &NONE, PRIMARY KEY (a, c));")
    ([:create-table foo ([a b c] (:unique [a b c]))] '()
     "CREATE TABLE foo (a &NONE, b &NONE, c &NONE, UNIQUE (a, b, c));")
    ([:create-table foo ([a b] (:check (< a b)))] '()
     "CREATE TABLE foo (a &NONE, b &NONE, CHECK (a < b));")
    ([:create-table foo ([a b c]
                         ( :foreign-key [a b]
                           :references bar [aa bb]
                           :on-delete :cascade))]
     '()
     (concat "CREATE TABLE foo (a &NONE, b &NONE, c &NONE, FOREIGN KEY (a, b) "
             "REFERENCES bar (aa, bb) ON DELETE CASCADE);"))
    ;; Template
    ([:create-table $i1 $S2] '(foo [alpha beta delta])
     "CREATE TABLE foo (alpha &NONE, beta &NONE, delta &NONE);")
    ;; Drop table
    ([:drop-table $i1] '(foo)
     "DROP TABLE foo;")))

(ert-deftest emacsql-update ()
  (emacsql-tests-with-queries
    ([:update people :set (= id $s1)] '(10)
     "UPDATE people SET id = 10;")))

(ert-deftest emacsql-insert ()
  (emacsql-tests-with-queries
    ([:insert :into foo :values [nil $s1]] '(10.1)
     "INSERT INTO foo VALUES (NULL, 10.1);")
    ([:insert :into foo [a b] :values $v1] '([1 2])
     "INSERT INTO foo (a, b) VALUES (1, 2);")
    ([:replace :into $i1 :values $v2] '(bar ([1 2] [3 4]))
     "REPLACE INTO bar VALUES (1, 2), (3, 4);")))

(ert-deftest emacsql-order-by ()
  (emacsql-tests-with-queries
    ([:order-by foo] '()
     "ORDER BY foo;")
    ([:order-by [$i1]] '(bar)
     "ORDER BY bar;")
    ([:order-by (- foo)] '()
     "ORDER BY -foo;")
    ([:order-by [(asc a) (desc (/ b 2))]] '()
     "ORDER BY a ASC, b / 2 DESC;")))

(ert-deftest emacsql-limit ()
  (emacsql-tests-with-queries
    ([:limit 10] '()
     "LIMIT 10;")
    ([:limit $s1] '(11)
     "LIMIT 11;")
    ([:limit [12]] '()
     "LIMIT 12;")
    ([:limit [2 10]] '()
     "LIMIT 2, 10;")
    ([:limit [$s1 $s2]] '(4 30)
     "LIMIT 4, 30;")))

(ert-deftest emacsql-quoting ()
  (emacsql-tests-with-queries
    ([:where (= name 'foo)] '()
     "WHERE name = 'foo';")
    ([:where (= name '$s1)] '(qux)
     "WHERE name = 'qux';")
    ([:where (like url (escape "%`%%" ?`))] '()
     "WHERE url LIKE '\"%`%%\"' ESCAPE '`';")))

(ert-deftest emacsql-expr ()
  (emacsql-tests-with-queries
    ([:where (and a b)] '()
     "WHERE a AND b;")
    ([:where (or a $i1)] '(b)
     "WHERE a OR b;")
    ([:where (and $i1 $i2 $i3)] '(a b c)
     "WHERE a AND b AND c;")
    ([:where (is foo (not nil))] '()
     "WHERE foo IS (NOT NULL);")
    ([:where (is-not foo nil)] '()
     "WHERE foo IS NOT NULL;")
    ([:where (= attrib :name)] '()
     "WHERE attrib = ':name';")))

(ert-deftest emacsql-transaction ()
  (emacsql-tests-with-queries
    ([:begin :transaction] '()
     "BEGIN TRANSACTION;")
    ([:begin :immediate] '()
     "BEGIN IMMEDIATE;")
    ([:rollback] '()
     "ROLLBACK;")
    ([:commit] '()
     "COMMIT;")))

(ert-deftest emacsql-alter-table ()
  (emacsql-tests-with-queries
    ([:alter-table foo :rename-to bar] '()
     "ALTER TABLE foo RENAME TO bar;")
    ([:alter-table $i1 :rename-to $i2] '(alpha beta)
     "ALTER TABLE alpha RENAME TO beta;")
    ([:alter-table foo :add-column size :integer :not-null] '()
     "ALTER TABLE foo ADD COLUMN size INTEGER NOT NULL;")))

(ert-deftest emacsql-funcall ()
  (emacsql-tests-with-queries
    ([:select (funcall count x)] '()
     "SELECT count(x);")
    ([:select (funcall count *)] '()
     "SELECT count(*);")
    ([:select (funcall group-concat x y)] '()
     "SELECT group_concat(x, y);")
    ([:select (funcall foobar :distinct x y)] '()
     "SELECT foobar(':distinct', x, y);")
    ([:select (funcall count :distinct x)] '()
     "SELECT count(DISTINCT x);")))

(ert-deftest emacsql-precedence ()
  (emacsql-tests-with-queries
    ([:select (<< (not (is x nil)) 4)] '()
     "SELECT (NOT x IS NULL) << 4;")
    ([:select (* 3 (+ (/ 14 2) (- 5 3)))] '()
     "SELECT 3 * (14 / 2 + (5 - 3));")
    ([:select (- (|| (~ x) y))] '()
     "SELECT -~x || y;")
    ([:select (funcall length (|| (* x x) (* y y) (* z z)))] '()
     "SELECT length((x * x) || (y * y) || (z * z));")
    ([:select (and (+ (<= x y) 1) (>= y x))] '()
     "SELECT (x <= y) + 1 AND y >= x;")
    ([:select (or (& (<= x (+ y 1) (- z)) 1) (>= x z y))] '()
     "SELECT (y + 1 BETWEEN x AND -z) & 1 OR z BETWEEN y AND x;")))

;;; emacsql-tests.el ends here
