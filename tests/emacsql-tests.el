;;; emacsql-tests.el --- test suite for EmacSQL -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'emacsql-compiler-tests)
(require 'emacsql-external-tests)

;; Print testing information
(cl-eval-when (load eval)
  (princ (format "\nTesting %d database(s): %S\n"
                 (length emacsql-tests-connection-factories)
                 (mapcar #'car emacsql-tests-connection-factories))))

(provide 'emacsql-tests)

;;; emacsql-tests.el ends here
