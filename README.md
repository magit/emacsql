# Emacsql

Emacsql is a high-level Emacs Lisp front-end for SQLite. It's
currently a work in progress.

It works by keeping a `sqlite3` inferior process running (a
"connection") for interacting with the back-end database. Connections
are automatically cleaned up if they are garbage collected. All
requests are synchronous.

Requires Emacs 24 or later.

```el
(defvar db (emacsql-connect "company.sqlite"))

;; Create a table. A table identifier can be any kind of lisp value.
(emacsql-create db :employees '(name id salary))

;; Or optionally provide type information:
(emacsql-create db :employees '((name text) (id integer) salary))

;; Insert some data:
(emacsql-insert db :employees ["Jeff"  1000 60000]
                              ["Susan" 1001 64000])

;; The high-level SELECT interface is a work in progress.
(emacsql-select-raw db (concat "SELECT name, id FROM ':employees' "
                               "WHERE salary > 60000;"))
;; => (((name . "Susan") (id . 1001)))
```

## Limitations

Due to limitations of the SQLite command line program, emacsql is
*not* intended to play well with other programs accessing the SQLite
database. Text values and blobs are stored encoded as s-expressions in
order to avoid ambiguities in parsing output from the command line.
This is a high-performance database specifically for Emacs.
