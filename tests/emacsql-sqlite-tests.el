;;; emacsql-external-tests.el --- subprocess tests  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'ert)
(require 'emacsql-sqlite)

(defvar emacsql-sqlite-tests-timeout 8
  "Be aggressive about not waiting on subprocesses in unit tests.")

(ert-deftest emacsql-sqlite-init-error-handling ()
  "Test emacsql-sqlite initialization error handling with malformed message."
  (let ((msg-malformed-send
         (lambda (old-fn connection _)
           (let ((process (oref connection handle)))
             ;; A well formed msg has the form `N STRING' where N is
             ;; the number of bytes in STRING.
             ;;
             ;; Msg starts with a letter instead of digit,
             ;; i.e. malformed.
             (process-send-string process "x\n")))))

    (advice-add 'emacsql-send-message :around msg-malformed-send)
    (unwind-protect
        (progn
          (let ((emacsql-global-timeout emacsql-sqlite-tests-timeout))
            (pcase-let ((`(,error-symbol ,data) (should-error (emacsql-sqlite nil))))
              (should (eq 'error error-symbol))
              ;; Expecting to get an error message of the form
              ;;   /.../emacsql-sqlite exited abnormallly :status exit :exit-code 1 :last-output error 1 \"middleware parsing error\"\n
              (should (string-match-p (regexp-quote ":exit-code 1 :last-output error 1 \"middleware parsing error\"") data)))))
      (advice-remove 'emacsql-send-message msg-malformed-send))))

(provide 'emacsql-sqlite-tests)

;;; emacsql-external-tests.el ends here
