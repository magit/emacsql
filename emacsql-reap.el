;;; emacsql-reap.el --- callbacks for garbage-collected objects -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Commentary:

;; This package is for immediately running a callback after a lisp
;; object is garbage collected. An optional token can be passed to the
;; callback to provide a hint as to what was collected, since the
;; original object obviously can't be passed.

;; It works by exploiting Emacs Lisp's weak hash tables and hooking
;; the `post-gc-hook'.

;;; Code:

(require 'cl-lib)

(defvar emacsql-reap-objects ()
  "Collection of all objects being watched by the reaper.")

(defun emacsql-reap--ref (thing)
  "Create a weak reference to THING."
  (let ((ref (make-hash-table :test 'eq :size 1 :weakness 'value)))
    (prog1 ref
      (setf (gethash t ref) thing))))

(defun emacsql-reap--deref (ref)
  "Retrieve value from REF."
  (gethash t ref))

(defun emacsql-reap--empty-p (ref)
  "Return non-nil if value behind REF is still there."
  (zerop (hash-table-count ref)))

(cl-defun emacsql-reap-register (object callback &optional (token nil token-p))
  "Run CALLBACK with TOKEN when OBJECT is garbage collected.
Do *not* use OBJECT for TOKEN because it will not get collected."
  (let ((ref (emacsql-reap--ref object))
        (rich-token (and token-p (vector token))))
    ;; Rich-token could be instead captured in a closure, but
    ;; establishing a closure here would require this package to be
    ;; byte-compiled in order to operate properly. Interpreted
    ;; closures capture the entire environment.
    (push (list callback rich-token ref) emacsql-reap-objects)))

(defun emacsql-reap-single (entry)
  "Cleanup ENTRY and return non-nil if ENTRY has been garbage collected."
  (cl-destructuring-bind (callback token ref) entry
    (when (emacsql-reap--empty-p ref)
      (prog1 t
        (ignore-errors
          (if token
              (funcall callback (elt token 0))
            (funcall callback)))))))

(defun emacsql-reap ()
  "Run callbacks for garbage collected objects."
  (setf emacsql-reap-objects
        (cl-delete-if #'emacsql-reap-single emacsql-reap-objects)))

(add-hook 'post-gc-hook #'emacsql-reap)

(provide 'emacsql-reap)

;;; emacsql-reap.el ends here
