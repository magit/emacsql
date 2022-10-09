;;; emacsql-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emacsql" "emacsql.el" (0 0 0 0))
;;; Generated autoloads from emacsql.el

(autoload 'emacsql-show-last-sql "emacsql" "\
Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

\(fn &optional PREFIX)" t nil)

(register-definition-prefixes "emacsql" '("emacsql-"))

;;;***

;;;### (autoloads nil "emacsql-compiler" "emacsql-compiler.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emacsql-compiler.el

(register-definition-prefixes "emacsql-compiler" '("emacsql-"))

;;;***

;;;### (autoloads nil "emacsql-mysql" "emacsql-mysql.el" (0 0 0 0))
;;; Generated autoloads from emacsql-mysql.el

(register-definition-prefixes "emacsql-mysql" '("emacsql-mysql-"))

;;;***

;;;### (autoloads nil "emacsql-pg" "emacsql-pg.el" (0 0 0 0))
;;; Generated autoloads from emacsql-pg.el

(register-definition-prefixes "emacsql-pg" '("emacsql-pg-connection"))

;;;***

;;;### (autoloads nil "emacsql-psql" "emacsql-psql.el" (0 0 0 0))
;;; Generated autoloads from emacsql-psql.el

(register-definition-prefixes "emacsql-psql" '("emacsql-psql-"))

;;;***

;;;### (autoloads nil "emacsql-sqlite" "emacsql-sqlite.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from emacsql-sqlite.el

(register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emacsql-autoloads.el ends here
