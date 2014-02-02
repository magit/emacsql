;;; emacsql-external-tests.el --- subprocess tests -*- lexical-binding: t; -*-

(require 'cl-lib)
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
        (emacsql db [:create-temporary-table test-table ([x])])
        (emacsql db [:insert-into test-table :values ([""] [\])])
        (should (process-live-p (emacsql-process db)))
        (should (equal (emacsql db [:select * :from test-table])
                       '(("") (\))))))))

(provide 'emacsql-external-tests)

;;; emacsql-external-tests.el ends here
