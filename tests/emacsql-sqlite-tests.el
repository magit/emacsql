;;; emacsql-sqlite-tests.el --- sqlite tests  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'ert)
(require 'emacsql-sqlite)

(ert-deftest emacsql-sqlite-init-error-handling ()
  "Test emacsql-sqlite initialization error handling with malformed message."
  (let ((msg-malformed-send
         (lambda (connection _)
           (let ((process (oref connection handle)))
             ;; Well formed messages have the form "N STRING", where N is
             ;; the number of bytes in STRING.  Here we use a malformated
             ;; message.
             (process-send-string process "x\n")))))
    (advice-add 'emacsql-send-message :override msg-malformed-send)
    (unwind-protect
        (let ((emacsql-global-timeout 8))
          (pcase-let ((`(,error-symbol ,data)
                       (should-error (emacsql-sqlite nil))))
            (should (eq 'emacsql-error error-symbol))
            (should (string-match-p
                     (regexp-quote "\
:exit-code 1 :last-output error 1 \"middleware parsing error\"") ;FIXME
                     data))))
      (advice-remove 'emacsql-send-message msg-malformed-send))))

(provide 'emacsql-sqlite-tests)

;;; emacsql-sqlite-tests.el ends here
