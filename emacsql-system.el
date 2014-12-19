;;; emacsql-system.el --- detect OS and machine -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defun emacsql-system-normalize-arch (arch)
  "Normalize the name of string ARCH."
  (cond ((string-match-p "^i[0-9]\\{3\\}" arch) "x86")
        ((string-match-p "^amd64" arch) "x86_64")
        (arch)))

(defun emacsql-system-normalize-os (os)
  "Normalize OS into a simple canonical name.
Unfortunately config.guess has lots of names for Windows."
  (cond ((string-match-p "^nt" os) "windows")
        ((string-match-p "^ming" os) "windows")
        ((string-match-p "^cygwin" os) "windows")
        ((string-match-p "^linux" os) "linux")
        ((string-match-p "^darwin" os) "darwin")
        (os)))

(defun emacsql-system-tuple ()
  "Determine the architecture-system tuple for Emacs' host system."
  (cl-destructuring-bind (arch _vendor . os-parts)
      (split-string system-configuration "-")
    (let ((os (mapconcat #'identity os-parts "-")))
      (format "%s-%s" (emacsql-system-normalize-arch arch)
              (emacsql-system-normalize-os os)))))

(defun emacsql-system-print-tuple ()
  "This is for calling from a Makefile."
  (princ (emacsql-system-tuple))
  (princ "\n"))

(provide 'emacsql-system)

;;; emacsql-system.el ends here
