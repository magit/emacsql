# Emacsql

Emacsql is a high-level Emacs Lisp front-end for SQLite. It's
currently a work in progress.

It works by keeping a `sqlite3` inferior process running (a
"connection") for interacting with the back-end database. Connections
are automatically cleaned up if they are garbage collected. All
requests are synchronous.

Any [readable lisp value][readable] can be stored as values in
Emacsql, including numbers, strings, symbols, lists, vectors, and
closures. Emacsql has no concept of "TEXT" values, it's all just lisp
objects.

Requires Emacs 24 or later.

```el
(defvar db (emacsql-connect "company.db"))

;; Create a table. A table identifier can be any kind of lisp value.
(emacsql-create db :employees [name id salary])

;; Or optionally provide column constraints.
(emacsql-create db :employees [name (id integer :unique) (salary float)])

;; Insert some data:
(emacsql-insert db :employees ["Jeff"  1000 60000.0]
                              ["Susan" 1001 64000.0])

;; The high-level SELECT interface is a work in progress.
(emacsql-select-raw db (concat "SELECT name, id FROM ':employees' "
                               "WHERE salary > 60000;"))
;; => (("Susan" 1001))
```

## Limitations

Emacsql is *not* intended to play well with other programs accessing
the SQLite database. Non-numeric values are are stored encoded as
s-expressions TEXT values. This avoids ambiguities in parsing output
from the command line and allows for storage of Emacs richer data
types. This is a high-performance database specifically for Emacs.


[readable]: http://nullprogram.com/blog/2013/12/30/#almost_everything_prints_readably
