# EmacSQL

EmacSQL is a high-level Emacs Lisp front-end for SQLite (primarily),
PostgreSQL, MySQL, and potentially other SQL databases.

It works by maintaining a inferior process running (a "connection")
for interacting with the back-end database. Connections are
automatically cleaned up if they are garbage collected. All requests
are synchronous.

Any [readable lisp value][readable] can be stored as a value in
EmacSQL, including numbers, strings, symbols, lists, vectors, and
closures. EmacSQL has no concept of "TEXT" values; it's all just lisp
objects. The lisp object `nil` corresponds 1:1 with `NULL` in the
database.

On MELPA, each backend is provided as a separate package, suffixed with
the database name. In the case of `emacsql-sqlite`, on first use EmacSQL
will attempt to find a C compiler and use it to compile a custom native
binary for communicating with a SQLite database.

Requires Emacs 25 or later.

### Windows Issues

Emacs `start-process-shell-command` function is not supported on
Windows. Since both `emacsql-mysql` and `emacsql-psql` rely on this
function, neither of these connection types are supported on Windows.

## Example Usage

```el
(defvar db (emacsql-sqlite "~/company.db"))

;; Create a table. Table and column identifiers are symbols.
(emacsql db [:create-table people ([name id salary])])

;; Or optionally provide column constraints.
(emacsql db [:create-table people
             ([name (id integer :primary-key) (salary float)])])

;; Insert some data:
(emacsql db [:insert :into people
             :values (["Jeff" 1000 60000.0] ["Susan" 1001 64000.0])])

;; Query the database for results:
(emacsql db [:select [name id]
             :from people
             :where (> salary 62000)])
;; => (("Susan" 1001))

;; Queries can be templates, using $1, $2, etc.:
(emacsql db [:select [name id]
             :from people
             :where (> salary $s1)]
         50000)
;; => (("Jeff" 1000) ("Susan" 1001))
```

When editing these prepared SQL s-expression statements, the `M-x
emacsql-show-last-sql` command (think `eval-last-sexp`) is useful for
seeing what the actual SQL expression will become when compiled.

## Schema

A table schema is a list whose first element is a vector of column
specifications. The rest of the list specifies table constraints. A
column identifier is a symbol and a column's specification can either
be just this symbol or it can include constraints as a list. Because
EmacSQL stores entire lisp objects as values, the only relevant (and
allowed) types are `integer`, `float`, and `object` (default).

    ([(<column>) ...] (<table-constraint> ...) ...])

Dashes in identifiers are converted into underscores when compiled
into SQL. This allows for lisp-style identifiers to be used in SQL.
Constraints follow the compilation rules below.

```el
;; No constraints schema with four columns:
([name id building room])

;; Add some column constraints:
([(name :unique) (id integer :primary-key) building room])

;; Add some table constraints:
([(name :unique) (id integer :primary-key) building room]
 (:unique [building room])
 (:check (> id 0)))
```

Here's an example using foreign keys.

```el
;; "subjects" table schema
([(id integer :primary-key) subject])

;; "tag" table references subjects
([(subject-id integer) tag]
 (:foreign-key [subject-id] :references subjects [id]
               :on-delete :cascade))
```

Foreign key constraints are enabled by default in EmacSQL.

## Operators

Expressions are written lisp-style, with the operator first. If it
looks like an operator EmacSQL treats it like an operator. However,
several operators are special.

    <=    >=    funcall    quote

The `<=` and `>=` operators accept 2 or 3 operands, transforming into
a SQL `_ BETWEEN _ AND _` operator as appropriate.

For function-like "operators" like `count` and `max` use the `funcall`
"operator."

```el
[:select (funcall max age) :from people]
```

Inside expressions, EmacSQL cannot tell the difference between symbol
literals and column references. If you're talking about the symbol
itself, just quote it as you would in normal Elisp. Note that this
does not "escape" `$tn` parameter symbols.

```el
(emacsql db [... :where (= category 'hiking)])
```

Quoting a string makes EmacSQL handle it as a "raw string." These raw
strings are not printed when being assembled into a query. These are
intended for use in special circumstances like filenames (`ATTACH`) or
pattern matching (`LIKE`). It is vital that raw strings are not
returned as results.

```el
(emacsql db [... :where (like name '"%foo%")])
(emacsql db [:attach '"/path/to/foo.db" :as foo])
```

Since template parameters include their type they never need to be
quoted.

With `glob` and `like` SQL operators keep in mind that they're
matching the *printed* representations of these values, even if the
value is a string.

The `||` concatenation operator is unsupported because concatenating
printed representations breaks an important constraint: all values must
remain readable within SQLite.

## Prepared Statements

The database is interacted with via prepared SQL s-expression
statements. You shouldn't normally be concatenating strings on your
own. (And it leaves out any possibility of a SQL injection!) See the
"Usage" section above for examples. A statement is a vector of
keywords and other lisp object.

Prepared EmacSQL s-expression statements are compiled into SQL
statements. The statement compiler is memoized so that using the same
statement multiple times is fast. To assist in this, the statement can
act as a template -- using `$i1`, `$s2`, etc. -- working like the
Elisp `format` function.

### Compilation Rules

Rather than the typical uppercase SQL keywords, keywords in a prepared
EmacSQL statement are literally just that: lisp keywords. EmacSQL only
understands a very small amount of SQL's syntax. The compiler follows
some simple rules to convert an s-expression into SQL.

#### All prepared statements are vectors.

A prepared s-expression statement is a vector beginning with a keyword
followed by a series of keywords and special values. This includes
most kinds of sub-queries.

```el
[:select ... :from ...]
[:select tag :from tags
 :where (in tag [:select ...])]
```

#### Keywords are split and capitalized.

Dashes are converted into spaces and the keyword gets capitalized. For
example, `:if-not-exists` becomes `IF NOT EXISTS`. How you choose to
combine keywords is up to your personal taste (e.g. `:drop :table` vs.
`:drop-table`).

#### Standalone symbols are identifiers.

EmacSQL doesn't know what symbols refer to identifiers and what
symbols should be treated as values. Use quotes to mark a symbol as a
value. For example, `people` here will be treated as an identifier.

```el
[:insert-into people :values ...]
```

#### Row-oriented information is always represented as vectors.

This includes rows being inserted, and sets of columns in a query. If
you're talking about a row-like thing then put it in a vector.

```el
[:select [id name] :from people]
```

Note that `*` is actually a SQL keyword, so don't put it in a vector.

```el
[:select * :from ...]
```

#### Lists are treated as expressions.

This is true even within row-oriented vectors.

```el
[... :where (= name "Bob")]
[:select [(/ seconds 60) count] :from ...]
```

Some things that are traditionally keywords -- particularly those that
are mixed in with expressions -- have been converted into operators
(`AS`, `ASC`, `DESC`).

```el
[... :order-by [(asc b), (desc a)]]   ; "ORDER BY b ASC, a DESC"
[:select p:name :from (as people p)]  ; "SELECT p.name FROM people AS p"
```

#### The `:values` keyword is special.

What follows `:values` is always treated like a vector or list of
vectors. Normally this sort of thing would appear to be a column
reference.

```el
[... :values [1 2 3]]
[... :values ([1 2 3] [4 5 6])]  ; insert multiple rows
```

#### A list whose first element is a vector is a table schema.

This is to distinguish schemas from everything else. With the
exception of what follows `:values`, nothing else is shaped like this.

```el
[:create-table people ([(id :primary-key) name])]
```

### Templates

To make statement compilation faster, and to avoid making you build up
statements dynamically, you can insert `$tn` parameters in place of
identifiers and values. These refer to the argument's type and its
argument position after the statement in the `emacsql` function,
one-indexed.

```el
(emacsql db [:select * :from $i1 :where (> salary $s2)] 'employees 50000)

(emacsql db [:select * :from employees :where (like name $r1)] "%Smith%")
```

The letter before the number is the type.

 * `i` : identifier
 * `s` : scalar
 * `v` : vector (or multiple vectors)
 * `r` : raw, unprinted strings
 * `S` : schema

When combined with `:values`, the vector type can refer to lists of
rows.

```el
(emacsql db [:insert-into favorite-characters :values $v1]
            '([0 "Calvin"] [1 "Hobbes"] [3 "Susie"]))
```

This is why rows must be vectors and not lists.

## SQLite Support

The custom EmacSQL SQLite binary is compiled with [Soundex][soundex] and
[full-text search][fts] (FTS3, FTS4, and FTS5) enabled -- features
disabled by the default SQLite build. This backend should work on any
system with a conforming ANSI C compiler installed under a command name
listed in `emacsql-sqlite-c-compilers`.

### Ignored Features

EmacSQL doesn't cover all of SQLite's features. Here are a list of
things that aren't supported, and probably will never be.

 * Collating. SQLite has three built-in collation functions: BINARY
   (default), NOCASE, and RTRIM. EmacSQL values never have right-hand
   whitespace, so RTRIM won't be of any use. NOCASE is broken
   (ASCII-only) and there's little reason to use it.

 * Text manipulation functions. Like collating this is incompatible
   with EmacSQL s-expression storage.

 * Date and time. These are incompatible with the printed values
   stored by EmacSQL and therefore have little use.

## Limitations

EmacSQL is *not* intended to play well with other programs accessing
the SQLite database. Non-numeric values are are stored encoded as
s-expressions TEXT values. This avoids ambiguities in parsing output
from the command line and allows for storage of Emacs richer data
types. This is an efficient, ACID-compliant database specifically for
Emacs.

## Emacs Lisp Indentation Annoyance

By default, `emacs-lisp-mode` indents vectors as if they were regular
function calls.

```el
;; Ugly indentation!
(emacsql db [:select *
                     :from people
                     :where (> age 60)])
```

Calling the function `emacsql-fix-vector-indentation` (interactive)
advises the major mode to fix this annoyance.

```el
;; Such indent!
(emacsql db [:select *
             :from people
             :where (> age 60)])
```

## Contributing and Extending

To run the test suite, clone the `pg` and `finalize` packages into
sibling directories. The Makefile will automatically put these paths on
the Emacs load path (override `LDFLAGS` if your situation is different).

    $ cd ..
    $ git clone https://github.com/cbbrowne/pg.el pg
    $ git clone https://github.com/skeeto/elisp-finalize finalize
    $ cd -

Then invoke make:

    $ make test

If the environment variable `PGDATABASE` is present then the unit
tests will also be run with PostgreSQL (emacsql-psql). Provide
`PGHOST`, `PGPORT`, and `PGUSER` if needed. If `PGUSER` is provided,
the pg.el backend (emacsql-pg) will also be tested.

If the environment variable `MYSQL_DBNAME` is present then the unit
tests will also be run with MySQL in the named database. Note that
this is not an official MySQL variable, just something made up for
EmacSQL.

### Creating a New Front-end

EmacSQL uses EIEIO so that interactions with a connection occur
through generic functions. You need to define a new class that
inherits from `emacsql-connection`.

 * Implement `emacsql-send-message`, `emacsql-waiting-p`,
   `emacsql-parse`, and `emacsql-close`.
 * Provide a constructor that initializes the connection and calls
   `emacsql-register` (for automatic connection cleanup).
 * Provide `emacsql-types` if needed (hint: use a class-allocated slot).
 * Ensure that you properly read NULL as nil (hint: ask your back-end
   to print it that way).
 * Register all reserved words with `emacsql-register-reserved`.
 * Preferably provide `emacsql-reconnect` if possible.
 * Set the default isolation level to *serializable*.
 * Enable autocommit mode by default.
 * Prefer ANSI syntax (value escapes, identifier escapes, etc.).
 * Enable foreign key constraints by default.

The goal of the autocommit, isolation, parsing, and foreign key
configuration settings is to normalize the interface as much as
possible. The connection's user should have the option to be agnostic
about which back-end is actually in use.

The provided implementations should serve as useful examples. If your
back-end outputs data in a clean, standard way you may be able to use
the emacsql-protocol-mixin class to do most of the work.

## See Also

 * [SQLite Documentation](https://www.sqlite.org/docs.html)


[readable]: http://nullprogram.com/blog/2013/12/30/#almost_everything_prints_readably
[stderr]: http://thread.gmane.org/gmane.comp.db.sqlite.general/85824
[foreign]: http://www.sqlite.org/foreignkeys.html
[batch]: http://lists.gnu.org/archive/html/emacs-pretest-bug/2005-11/msg00320.html
[fts]: http://www.sqlite.org/fts3.html
[soundex]: http://www.sqlite.org/compile.html#soundex
