;;; emacsql-external-tests.el --- subprocess tests  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'emacsql-psql)
(require 'emacsql-mysql)
(when (require 'pg nil t) (require 'emacsql-pg))

(defvar emacsql-tests-timeout 4
  "Be aggressive about not waiting on subprocesses in unit tests.")

(defvar emacsql-tests-connection-factories
  (let ((factories ())
        (psql-database (getenv "PSQL_DATABASE"))
        (pg-database (getenv "PG_DATABASE"))
        (pg-user (getenv "PG_USER"))
        (pg-password (getenv "PG_PASSWORD"))
        (pg-host (getenv "PG_HOST"))
        (pg-port (getenv "PG_PORT"))
        (mysql-database (getenv "MYSQL_DATABASE"))
        (mysql-user (getenv "MYSQL_USER"))
        (mysql-password (getenv "MYSQL_PASSWORD"))
        (mysql-host (getenv "MYSQL_HOST"))
        (mysql-port (getenv "MYSQL_PORT")))
    (cl-labels ((reg (name &rest args)
                  (push (cons name (apply #'apply-partially args)) factories)))
      (reg "sqlite" #'emacsql-sqlite nil)
      (when psql-database
        (reg "psql" #'emacsql-psql psql-database))
      (when (and pg-database pg-user pg-password pg-host pg-port (fboundp 'emacsql-pg))
        (reg "pg" #'emacsql-pg pg-database pg-user :host pg-host :password pg-password :port pg-port))
      (when (and mysql-database mysql-user mysql-host mysql-password mysql-port)
        (reg "mysql" #'emacsql-mysql mysql-database mysql-user :host mysql-host :password mysql-password :port mysql-port)))
    (nreverse factories))
  "List of connection factories to use in unit tests.")

(ert-deftest emacsql-basic ()
  "A short test that fully interacts with SQLite."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (emacsql db [:create-temporary-table foo ([x])])
        (should-error (emacsql db [:create-temporary-table foo ([x])]))
        (emacsql db [:insert :into foo :values ([1] [2] [3])])
        (should (equal (emacsql db [:select * :from foo])
                       '((1) (2) (3))))))))

(ert-deftest emacsql-nul-character ()
  "Try inserting and retrieving strings with a NUL byte."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (emacsql db [:create-temporary-table foo ([x])])
        (emacsql db [:insert :into foo :values (["a\0bc"])])
        (should (equal (emacsql db [:select * :from foo])
                       '(("a\0bc"))))))))

(ert-deftest emacsql-foreign-key ()
  "Tests that foreign keys work properly through EmacSQL."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall (cdr factory)))
        (unwind-protect
            (progn
              (emacsql-thread db
                [:create-table person ([(id integer :primary-key) name])]
                [:create-table likes
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
                             '((1 yellow)))))
          (emacsql-thread db
            [:drop-table likes]
            [:drop-table person]))))))

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
        (emacsql db [:create-temporary-table test-table ([x])])
        (emacsql db [:insert-into test-table :values ([""] [\])])
        (should (process-live-p (emacsql-process db)))
        (should (equal (emacsql db [:select * :from test-table])
                       '(("") (\))))))))

(provide 'emacsql-external-tests)

;;; emacsql-external-tests.el ends here
