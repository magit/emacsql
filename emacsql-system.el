;;; emacsql-system.el --- detect OS and machine -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun emacsql-system-normalize-arch (arch)
  "Normalize the name of string ARCH."
  (cl-case (intern arch)
    ((x86 i386 i486 i586 i686) 'x86)
    ((x86_64 amd64) 'x86_64)
    (otherwise (intern arch))))

(defun emacsql-system-architecture ()
  "Determine this system's architecture."
  (emacsql-system-normalize-arch
    (if (executable-find "uname")
        (with-temp-buffer
          (call-process "uname" nil (current-buffer) nil "-m")
          (replace-regexp-in-string "\\s " "" (buffer-string)))
      (getenv "PROCESSOR_ARCHITECTURE"))))

(defun emacsql-system-tuple ()
  "Return a tuple (kernel architecture) for the current system."
  (list
   (cl-ecase system-type
     (gnu 'hurd)
     (gnu/linux 'linux)
     ((gnu/kfreebsd berkeley-unix) 'bsd)
     (darwin 'darwin)
     (ms-dos 'dos)
     (windows-nt 'windows)
     (cygwin 'windows))
   (emacsql-system-architecture)))

(defun emacsql-system-binary (prefix)
  "Determine an executable name for PREFIX."
  (concat prefix "-" (mapconcat #'symbol-name (emacsql-system-tuple) "-")))

(provide 'emacsql-system)

;;; emacsql-system.el ends here
