;;; emacsql-tests.el --- tests for emacsql -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'emacsql-psql)

(defvar emacsql-tests-timeout 4
  "Be aggressive about not waiting on subprocesses in unit tests.")

(defvar emacsql-tests-connection-factories
  (let ((factories ())
        (pgdatabase (getenv "PGDATABASE")))
    (push (cons "sqlite" (apply-partially #'emacsql-sqlite nil)) factories)
    (when pgdatabase
      (push (cons "psql" (apply-partially #'emacsql-psql pgdatabase))
            factories))
    (nreverse factories))
  "List of connection factories to use in unit tests.")

;; Print testing information
(princ (format "\nTesting %d database(s): %S\n"
               (length emacsql-tests-connection-factories)
               (mapcar #'car emacsql-tests-connection-factories)))

(ert-deftest emacsql-escape-identifier ()
  (should-error (emacsql-escape-identifier "foo"))
  (should (string= (emacsql-escape-identifier 'foo) "foo"))
  (should (string= (emacsql-escape-identifier 'a\ b) "\"a\\ b\""))
  (should (string= (emacsql-escape-identifier '$foo) "\"$foo\""))
  (should-error (emacsql-escape-identifier 10))
  (should-error (emacsql-escape-identifier nil))
  (should (string= (emacsql-escape-identifier 'person-id) "person_id"))
  (should (string= (emacsql-escape-identifier
                    'people:person-id) "people.person_id"))
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
                         (:foreign-key [a b] :references bar [aa bb]
                                       :on-delete :cascade))] '()
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
     "ORDER BY -(foo);")
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
     "WHERE name = 'qux';")))

(ert-deftest emacsql-expr ()
  (emacsql-tests-with-queries
    ([:where (and a b)] '()
     "WHERE a AND b;")
    ([:where (or a $i1)] '(b)
     "WHERE a OR b;")
    ([:where (and $i1 $i2 $i3)] '(a b c)
     "WHERE a AND b AND c;")))

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

(ert-deftest emacsql-system ()
  "A short test that fully interacts with SQLite."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (emacsql db [:create-temporary-table foo ([x])])
        (should-error (emacsql db [:create-temporary-table foo ([x])]))
        (emacsql db [:insert :into foo :values ([1] [2] [3])])
        (should (equal (emacsql db [:select * :from foo])
                       '((1) (2) (3))))))))

(ert-deftest emacsql-foreign-system ()
  "Tests that foreign keys work properly through Emacsql."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (emacsql-thread db
          [:create-temporary-table person ([(id integer :primary-key) name])]
          [:create-temporary-table likes
           ([(personid integer) color]
            (:foreign-key [personid] :references person [id]
                          :on-delete :cascade))]
          [:insert :into person :values ([0 "Chris"] [1 "Brian"])])
        (should (equal (emacsql db [:select * :from person :order-by id])
                       '((0 "Chris") (1 "Brian"))))
        (emacsql db [:insert :into likes
                             :values ([0 red] [0 yellow] [1 yellow])])
        (should (equal (emacsql db [:select * :from likes
                                            :order-by [personid color]])
                       '((0 red) (0 yellow) (1 yellow))))
        (emacsql db [:delete :from person :where (= id 0)])
        (should (equal (emacsql db [:select * :from likes])
                       '((1 yellow))))))))

(ert-deftest emacsql-error ()
  "Check that we're getting expected conditions."
  (should-error (emacsql-compile nil [:insert :into foo :values 1])
                :type 'emacsql-syntax)
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (emacsql db [:create-temporary-table foo ([x])])
        (should-error (emacsql db [:create-temporary-table foo ([x])])
                      :type 'emacsql-error)))))

(ert-deftest emacsql-special-chars ()
  "A short test that interacts with SQLite with special characters."
  (let ((emacsql-global-timeout 4))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (emacsql db [:create-temporary-table test-table [x]])
        (emacsql db [:insert :into test-table :values ([""] [\])])
        (should (process-live-p (emacsql-process db)))
        (should (equal (emacsql db [:select * :from test-table])
                       '(("") (\))))))))

(provide 'emacsql-tests)

;;; emacsql-tests.el ends here
