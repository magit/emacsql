;;; emacsql-external-tests.el --- Subprocess tests  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emacsql)

(defvar emacsql-tests-timeout 4
  "Be aggressive about not waiting on subprocesses in unit tests.")

(defvar emacsql-tests-connection-factories nil
  "List of connection factories to use in unit tests.")

(defun emacsql-tests-add-connection-factory
    (connector &optional dep min pred envvars)
  (declare (indent defun))
  (cond
   ((and min (version< emacs-version min))
    (message " ! skip `%s'; requires Emacs >= %s" connector min))
   ((and dep (not (with-demoted-errors "%S" (require dep nil t))))
    (message " ! skip `%s'; library `%s' not available" connector dep))
   ((and pred (not (funcall pred)))
    (message " ! skip `%s'; sanity check failed" connector))
   ((not (with-demoted-errors "%S" (require connector nil t)))
    (message " ! skip `%s'; failed to load library" connector))
   ((let* ((unset ())
           (args (if envvars
                     (mapcan (lambda (var)
                               (let* ((envvar (car var))
                                      (keyword (cadr var))
                                      (value (and envvar (getenv envvar))))
                                 (cond ((not value) (push envvar unset) nil)
                                       (keyword (list keyword value))
                                       ((list value)))))
                             envvars)
                   (list nil))))
      (if unset
          (message " ! skip `%s'; required envvars not set" connector)
        (message "   test `%s' connector" connector)
        (push (apply #'apply-partially connector args)
              emacsql-tests-connection-factories))))))

(cl-eval-when (load eval)
  (emacsql-tests-add-connection-factory 'emacsql-sqlite-builtin 'sqlite "29.1"
    'sqlite-available-p)

  (emacsql-tests-add-connection-factory 'emacsql-sqlite-module 'sqlite3 nil
    (lambda () (boundp 'module-file-suffix)))

  (emacsql-tests-add-connection-factory 'emacsql-mysql nil nil nil
    '(("MYSQL_DATABASE")
      ("MYSQL_USER" :user)
      ("MYSQL_PASSWORD" :password)
      ("MYSQL_HOST" :host)
      ("MYSQL_PORT" :port)))

  (emacsql-tests-add-connection-factory 'emacsql-psql nil nil nil
    '(("PSQL_DATABASE")
      ("PSQL_USER" :username)
      ("PSQL_HOST" :hostname)
      ("PSQL_PORT" :port)))

  (message " ! skip `emacsql-pg' connector; known to be broken")
  ;; FIXME Fix broken `emacsql-pg'.
  ;; (emacsql-tests-add-connection-factory 'emacsql-pg 'pg "28.1" nil
  ;;   '(("PG_DATABASE")
  ;;     ("PG_USER")
  ;;     ("PG_PASSWORD" :password)
  ;;     ("PG_HOST" :host)
  ;;     ("PG_PORT" :port)))
  )

(ert-deftest emacsql-basic ()
  "A short test that fully interacts with SQLite."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall factory))
        (emacsql db [:create-temporary-table foo ([x])])
        (should-error (emacsql db [:create-temporary-table foo ([x])]))
        (emacsql db [:insert :into foo :values ([1] [2] [3])])
        (should (equal (emacsql db [:select * :from foo])
                       '((1) (2) (3))))))))

(ert-deftest emacsql-nul-character ()
  "Try inserting and retrieving strings with a NUL byte."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall factory))
        (emacsql db [:create-temporary-table foo ([x])])
        (emacsql db [:insert :into foo :values (["a\0bc"])])
        (should (equal (emacsql db [:select * :from foo])
                       '(("a\0bc"))))))))

(ert-deftest emacsql-foreign-key ()
  "Tests that foreign keys work properly through EmacSQL."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall factory))
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
      (emacsql-with-connection (db (funcall factory))
        (emacsql db [:create-temporary-table foo ([x])])
        (should-error (emacsql db [:create-temporary-table foo ([x])])
                      :type 'emacsql-error)))))

(ert-deftest emacsql-special-chars ()
  "A short test that interacts with SQLite with special characters."
  (let ((emacsql-global-timeout 4))
    (dolist (factory emacsql-tests-connection-factories)
      (emacsql-with-connection (db (funcall factory))
        (emacsql db [:create-temporary-table test-table ([x])])
        (emacsql db [:insert-into test-table :values ([""] [\])])
        (when (cl-typep db 'process)
          (should (emacsql-live-p db)))
        (should (equal (emacsql db [:select * :from test-table])
                       '(("") (\))))))))

;;; emacsql-external-tests.el ends here
