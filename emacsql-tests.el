;;; emacsql-tests.el --- tests for emacsql -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)

(ert-deftest emacsql-escape-identifier ()
  (should (string= (emacsql-escape-identifier "foo") "foo"))
  (should (string= (emacsql-escape-identifier 'foo) "foo"))
  (should (string= (emacsql-escape-identifier :foo) "foo"))
  (should-error (emacsql-escape-identifier "a b"))
  (should-error (emacsql-escape-identifier '$foo))
  (should-error (emacsql-escape-identifier 10))
  (should (string= (emacsql-escape-identifier 'person-id) "person_id"))
  (should (string= (emacsql-escape-identifier
                    'people:person-id) "people.person_id"))
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
  (should (string= (car (emacsql--schema-to-string [a]))
                   "a NONE"))
  (should (string= (car (emacsql--schema-to-string [a b c]))
                   "a NONE, b NONE, c NONE"))
  (should (string= (car (emacsql--schema-to-string [a (b)]))
                   "a NONE, b NONE"))
  (should (string= (car (emacsql--schema-to-string [a (b float)]))
                   "a NONE, b REAL"))
  (should (string= (car (emacsql--schema-to-string
                         [a (b float :primary :unique)]))
                   "a NONE, b REAL PRIMARY KEY UNIQUE"))
  (should (string= (car (emacsql--schema-to-string [(a integer) (b float)]))
                   "a INTEGER, b REAL")))

(ert-deftest emacsql-var ()
  (should (eq (emacsql-var 'a) nil))
  (should (eq (emacsql-var 0) nil))
  (should (eq (emacsql-var "") nil))
  (should (eq (emacsql-var '$) 0))
  (should (eq (emacsql-var '$1) 0))
  (should (eq (emacsql-var '$5) 4))
  (should (eq (emacsql-var '$10) 9))
  (should (eq (emacsql-var '$a) nil))
  (should (eq (emacsql-var '$$10) '$10)))

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
     "SELECT * FROM people WHERE name IN ('FOO', 'BAR');")
    ;; Sub queries
    ([:select name :from (:select * :from $1)] '(people)
     "SELECT name FROM (SELECT * FROM people);")
    ([:select name :from [people (as accounts a)]] '()
     "SELECT name FROM people, accounts AS a;")
    ([:select p:name :from [(as (:select * :from people) p)]] '()
     "SELECT p.name FROM (SELECT * FROM people) AS p;")))

(ert-deftest emacsql-create-table ()
  (emacsql-tests-with-queries
    ([:create-table foo [a b c]] ()
     "CREATE TABLE foo (a NONE, b NONE, c NONE);")
    ([:create-table (:temporary :if-not-exists x) [y]] '()
     "CREATE TEMPORARY TABLE IF NOT EXISTS x (y NONE);")
    ([:create-table foo [(a :default 10)]] '()
     "CREATE TABLE foo (a NONE DEFAULT 10);")
    ([:create-table foo [(a :primary :non-nil) b]] '()
     "CREATE TABLE foo (a NONE PRIMARY KEY NOT NULL, b NONE);")
    ([:create-table foo [a (b :check (< b 10))]] '()
     "CREATE TABLE foo (a NONE, b NONE CHECK (b < 10));")
    ([:create-table foo $1] '([a b (c :primary)])
     "CREATE TABLE foo (a NONE, b NONE, c NONE PRIMARY KEY);")
    ([:create-table foo [a b (c :default $1)]] '("FOO")
     "CREATE TABLE foo (a NONE, b NONE, c NONE DEFAULT '\"FOO\"');")
    ;; From select
    ([:create-table $1 [:select name :from $2]] '(names people)
     "CREATE TABLE names AS (SELECT name FROM people);")
    ;; Table constraints
    ([:create-table foo ([a b c] :primary [a c])] '()
     "CREATE TABLE foo (a NONE, b NONE, c NONE, PRIMARY KEY (a, c));")
    ([:create-table foo ([a b c] :unique [a b c])] '()
     "CREATE TABLE foo (a NONE, b NONE, c NONE, UNIQUE (a, b, c));")
    ([:create-table foo ([a b] :check (< a b)) ] '()
     "CREATE TABLE foo (a NONE, b NONE, CHECK (a < b));")
    ([:create-table foo
      ([a b c] :foreign ([a b] bar [aa bb] :on-delete :cascade))] '()
      (concat "CREATE TABLE foo (a NONE, b NONE, c NONE, FOREIGN KEY (a, b) "
              "REFERENCES bar (aa, bb) ON DELETE CASCADE);"))
    ;; Drop table
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

(ert-deftest emacsql-quoting ()
  (emacsql-tests-with-queries
    ([:where (= name 'foo)] '()
     "WHERE name = 'foo';")
    ([:where (= name '$1)] '(qux)
     "WHERE name = 'qux';")
    ([:where (= name '$$1)] '()
     "WHERE name = '$1';")
    ([:values [a $$1]] '()
     "VALUES ('a', '$1');")))

(ert-deftest emacsql-expr ()
  (emacsql-tests-with-queries
    ([:where (and)] '()
     "WHERE 1;")
    ([:where (or)] '()
     "WHERE 0;")
    ([:where (and a b)] '()
     "WHERE a AND b;")
    ([:where (or a $1)] '(b)
     "WHERE a OR b;")
    ([:where (and $1 $2 $3)] '(a b c)
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
   ([:alter-table $1 :rename-to $2] '(alpha beta)
    "ALTER TABLE alpha RENAME TO beta;")
   ([:alter-table foo :add-column ($1 integer :non-nil)] '(size)
    "ALTER TABLE foo ADD COLUMN size INTEGER NOT NULL;")))

(ert-deftest emacsql-system ()
  "A short test that fully interacts with SQLite."
  (should-not (emacsql-sqlite3-unavailable-p))
  (emacsql-with-connection (db (emacsql-sqlite nil))
    (emacsql db [:create-table foo [x]])
    (should-error (emacsql db [:create-table foo [x]]))
    (emacsql db [:insert :into foo :values ([1] [2] [3])])
    (should (equal (emacsql db [:select * :from foo])
                   '((1) (2) (3))))))

(ert-deftest emacsql-foreign-system ()
  "Tests that foreign keys work properly through Emacsql."
  (emacsql-with-connection (db (emacsql-sqlite nil))
    (emacsql-thread db
      [:create-table person [(id integer :primary) name]]
      [:create-table likes ([(personid integer) color]
                            :foreign (personid person id :on-delete :cascade))]
      [:replace :into person :values ([0 "Chris"] [1 "Brian"])])
    (should (equal (emacsql db [:select * :from person :order-by id])
                   '((0 "Chris") (1 "Brian"))))
    (emacsql db [:insert :into likes :values ([0 red] [0 yellow] [1 yellow])])
    (should (equal (emacsql db [:select * :from likes
                                        :order-by [personid color]])
                   '((0 red) (0 yellow) (1 yellow))))
    (emacsql db [:delete :from person :where (= id 0)])
    (should (equal (emacsql db [:select * :from likes])
                   '((1 yellow))))))

(ert-deftest emacsql-error ()
  "Check that we're getting expected conditions."
  (should-error (emacsql-compile nil [:begin :foo])
                :type 'emacsql-syntax)
  (should-error (emacsql-compile nil [:create-table $foo$ [a]])
                :type 'emacsql-syntax)
  (should-error (emacsql-compile nil [:insert :into foo :values 1])
                :type 'emacsql-syntax)
  (emacsql-with-connection (db (emacsql-sqlite nil))
    (emacsql db [:create-table foo [x]])
    (should-error (emacsql db [:create-table foo [x]])
                  :type 'emacsql-table)))

(provide 'emacsql-tests)

;;; emacsql-tests.el ends here
