;;; emacsql-sqlite-common.el --- Transitional library that should not be loaded  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Jonas Bernoulli <emacs.emacsql@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.emacsql@jonas.bernoulli.dev>

;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; Transitional library that should not be loaded.  If your package still
;; requires this library, change it to require `emacsql-sqlite' instead.

;;; Code:

(require 'emacsql-sqlite)

(provide 'emacsql-sqlite-common)

;;; emacsql-sqlite-common.el ends here

