# Emacsql

Emacsql is a high-level Emacs Lisp front-end for SQLite (primarily),
PostgreSQL, and potentially other SQL databases. It is currently a
work-in-progress (90% complete) because the s-expression query
language is still being hammered out.

It works by keeping a `sqlite3` (or `psql`) inferior process running
(a "connection") for interacting with the back-end database.
Connections are automatically cleaned up if they are garbage
collected. All requests are synchronous.

Any [readable lisp value][readable] can be stored as a value in
Emacsql, including numbers, strings, symbols, lists, vectors, and
closures. Emacsql has no concept of "TEXT" values; it's all just lisp
objects. The lisp object `nil` corresponds 1:1 with `NULL` in the
database.

Requires Emacs 24 or later.

### Windows Issues

Due to [bad behavior from both SQLite and Windows][stderr] the
official SQLite command shell binary will *not* work with Emacsql on
Windows. Fortunately, [this simple patch][patch] corrects the issue.
Here's a build with the fix:

 * [sqlite3.exe][exe] (3.8.2, [asc][asc])

Also, due to a [long-standing Emacs bug][batch], Emacsql cannot be
used in Emacs' "-batch" mode on Windows, which includes running the
Emacsql test suite from the Makefile.

## Example Usage

```el
(defvar db (emacsql-connect "~/company.db"))

;; Create a table. Table and column identifiers are symbols.
(emacsql db [:create-table people [name id salary]])

;; Or optionally provide column constraints.
(emacsql db [:create-table people [name (id integer :unique) (salary float)]])

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
             :where (> salary $1)]
         50000)
;; => (("Jeff" 1000) ("Susan" 1001))
```

When editing these structured SQL statements, the `M-x
emacsql-show-last-sql` command (think `eval-last-sexp`) is useful for
seeing what the actual SQL expression will become when compiled.

## Schema

A table schema is a vector of column specifications, or a list
containing a vector of column specifications followed by table
specifications. A column identifier is a symbol and a specification
can either be just this symbol or it can include constraints. Because
Emacsql stores entire lisp objects as values, the only relevant (and
allowed) types are `integer`, `float`, and `object` (default).

Columns constraints include `:primary` (aka `PRIMARY KEY`),
`:autoincrement`, `:unique`, `:non-nil` (aka `NOT NULL`), `:default`,
and `:check`.

Table constraints can be `:primary`, `:unique`, `:check`, and `:foreign`.

```el
;; No constraints schema with four columns:
[name id building room]

;; Add some column constraints:
[(name :unique) (id integer :primary) building room]

;; Add some table constraints:
([(name :unique) (id integer :primary) building room]
 :unique [building room] :check ())
```

Foreign keys are the most complex. Action triggers are `:on-delete`
or `:on-update` and possible actions are `:set-nil`, `:set-default`,
`:restrict`, `:cascade`. See [the SQLite documentation][foreign] for
the details on what each of these means.

`:foreign (<child-keys> <parent-table> <parent-keys> [<actions>])`.

```el
;; "subject" table
[(id integer :primary) subject]

;; "tag" table references subjects
([(subjectid integer) tag]
 :foreign (subjectid subject id :on-delete :cascade))
```

Put the keys in a vector if the reference is composite. Also remember
that foreign key checks are currently disabled by default in SQLite,
so you'll need to enable it for each connection.

```el
(emacsql db [:pragma (= foreign_keys on)])
```

## Operators

Emacsql supports the following SQLite expression operators, named
exactly like so in a structured Emacsql statement.

    *     /     %     +     -     <<    >>    &
    |     <     <=    >     >=    =     !=
    is    like  glob  and   or    in

In addition, Emacsql has these operators.

    quote   as    not

The `<=` and `>=` operators accept 2 or 3 operands, transforming into
a SQL `_ BETWEEN _ AND _` operator as appropriate.

With `glob` and `like` keep in mind that they're matching the
*printed* representations of these values, even if the value is a
string.

Inside expressions, Emacsql cannot tell the difference between symbol
literals and column references. If you're talking about the symbol
itself, just quote it as you would in normal Elisp. Note that this
does not "escape" `$n` variables: it just means the argument gets
quoted. Use `$$` for escaping variables.

```el
[... :where (= category 'hiking)]
(emacsql db [... :where (= category '$1)] 'hiking)
```

The `||` concatenation operator is unsupported because concatenating
printed representations breaks an important constraint: all values must
remain readable within SQLite.

## Structured Statements

The database is interacted with via structured s-expression
statements. You won't be concatenating strings on your own. (And it
leaves out any possibility of a SQL injection!) See the "Usage"
section above for examples. A statement is a vector of keywords and
other lisp object.

Structured Emacsql statements are compiled into SQL statements. The
statement compiler is memoized so that using the same statement
multiple times is fast. To assist in this, the statement can act as a
template -- using `$1`, `$2`, etc. -- working like the Elisp `format`
function.

### Keywords

Rather than the typical uppercase SQL keywords, keywords in a
structured Emacsql statement are literally just that: lisp keywords.
When multiple keywords appear in sequence, Emacsql will generally
concatenate them with a dash, e.g. `CREATE TABLE` becomes
`:create-table`.

#### :create-table `<table>` `<schema|select>`

Provides `CREATE TABLE`. A selection can be used in place of a schema,
which will create a `CREATE TABLE ... AS` statement.

```el
[:create-table employees [name (id integer :primary) (salary float)]]
[:create-table (:temporary :if-not-exists employees) ...]
[:create-table names [:select name :from employees]]
```

#### :drop-table `<table>`

Provides `DROP TABLE`.

```el
[:drop-table employees]
```

#### :select `<column-spec>|(:distinct <column-spec>)`

Provides `SELECT`. `column-spec` can be a `*` symbol or a vector of
column identifiers, optionally as expressions.

```el
[:select [name (/ salary 52)] ...]
[:select [(as name n) (as age a)] ...]
[:select (:distinct [name age id]) ...]
```

#### :from `<table>`

Provides `FROM`.

```el
[... :from employees]
[... :from [employees accounts]]
[... :from [employees (as accounts a)]]
[... :from (:select ...)]
[... :from [(as (:select ...) s1) (as (:select ...) s2)]]
```

#### :where `<expr>`, :having `<expr>`

Provides `WHERE` and `HAVING`.

```el
[... :where (< count 10)]
[... :having (= size 10)]
```

#### :group-by `<expr>`

Provides `GROUP BY`.

```el
[... :group-by name]
```

#### :order-by `<expr>|(<expr> <:asc|:desc>)|[<expr> ...]`

Provides `ORDER BY`.

```el
[... :order-by date]
[... :order-by [(width :asc) (height :desc)]]
[... :order-by [(width :asc) (- height)]]
```

#### :limit `<limit>|[<offset> <limit>]`

Provides `LIMIT` and `OFFSET`.

```el
[... :limit 50]
[... :limit [150 50]]
```

#### :insert, :replace

Provides `INSERT`, `REPLACE`.

```el
[:insert :into ...]
[:replace :into ...]
```

#### :into `<table>`

Provides `INTO`.

```el
[:into employees ...]
[:into (employees [id name]) ...]
```

#### :delete

Provides `DELETE`.

```el
[:delete :from employees :where ...]
```

#### :values `<vector>|(<vector> ...)`

```el
[:insert :into employees :values ["Jeff" 0]]
[:insert :into employees :values (["Jeff" 0] ["Susan" 0])]
```

#### :update `<table>`

Provides `UPDATE`.

```el
[:update people :set ...]
```

#### :set `<assignment>|[<assignment> ...]`

Provides `SET`.

```el
[:update people :set (= name "Richy") :where ...]
[:update people :set [(= name "Richy") (= salary 300000)] :where ...]
```

#### :union, :union-all, :difference, :except

Provides `UNION`, `UNION ALL`, `DIFFERENCE`, and `EXCEPT`.

```el
[:select * :from sales :union :select * :from accounting]
```

#### :begin `<:transaction|:immediate|:deferred|:exclusive>`

Provides `BEGIN`. Exactly one of these "arguments" must always be
supplied. `:deferred` and `:transaction` are aliases.

```el
[:begin :transaction]
[:begin :immediate]
```

#### :commit, :rollback

Provides `COMMIT` and `ROLLBACK`.

```el
[:commit]
[:rollback]
```

#### :alter-table `<table>`, :rename-to `<table>`

Provides `ALTER TABLE` and `RENAME TO`.

```el
[:alter-table prices :rename-to costs]
```

### :add-column `<column-spec>`

Provides `ADD COLUMN`.

```el
[:alter-table tags :add-column (rating integer :non-nil)]
```

#### :pragma `<expr>`

Provides `PRAGMA`.

```el
(emacsql db [:pragma (= foreign_keys on)])
```

#### :vacuum

Provides `VACUUM`.

### Templates

To make statement compilation faster, and to avoid making you build up
statements dynamically, you can insert `$n` "variables" in place of
identifiers and values. These refer to argument positions after the
statement in the `emacsql` function, 1-indexed.

    (emacsql db [:select * :from $1 :where (> salary $2)] 'employees 50000)

To get a literal symbol that looks like one of these variables, escape
it with an extra dollar sign (i.e. `$$1` becomes `$1`).

## Ignored Features

Emacsql doesn't cover all of SQLite's features, focusing on the most
important syntax. Here are a list of things that aren't supported, and
probably will never be.

 * Collating. SQLite has three built-in collation functions: BINARY
   (default), NOCASE, and RTRIM. Emacsql values never have right-hand
   whitepsace, so RTRIM won't be of any use. NOCASE is broken
   (ASCII-only) and there's little reason to use it.

 * Databases attachments. I don't expect any program using Emacsql to
   become so complex as to warrant multiple databases. Ignoring this
   removes unneeded complexity.

 * Date and time. These are incompatible with the printed values
   stored by Emacsql and therefore have little use.

## Limitations

Emacsql is *not* intended to play well with other programs accessing
the SQLite database. Non-numeric values are are stored encoded as
s-expressions TEXT values. This avoids ambiguities in parsing output
from the command line and allows for storage of Emacs richer data
types. This is an efficient, ACID-compliant database specifically for
Emacs.

## Creating a New Front-end

Emacsql uses EIEIO so that interactions with a connection occur
through generic functions. You need to define a new class that
inherits from `emacsql-connection`.

 * Implement `emacsql-waiting-p`, `emacsql-parse`, and `emacsql-close`.
 * Provide a constructor that initializes the connection and calls
   `emacsql-register` (for automatic connection cleanup).
 * Provide `emacsql-types` if needed (hint: use a class-allocated slot).
 * Ensure that you properly read NULL as nil (hint: ask your back-end
   to print it that way).

The provided implementations should serve as useful examples. If your
back-end outputs data in a clean, standard way you may be able to use
the emacsql-simple-parser mixin class to do most of the work.

## See Also

 * [SQLite Documentation](https://www.sqlite.org/docs.html)


[readable]: http://nullprogram.com/blog/2013/12/30/#almost_everything_prints_readably
[stderr]: http://thread.gmane.org/gmane.comp.db.sqlite.general/85824
[foreign]: http://www.sqlite.org/foreignkeys.html
[patch]: http://skeeto.s3.amazonaws.com/emacs/sqlite3-stderr-fix.patch
[exe]: http://skeeto.s3.amazonaws.com/emacs/sqlite3-3.8.2-fixed.exe
[asc]: http://skeeto.s3.amazonaws.com/emacs/sqlite3-3.8.2-fixed.exe.asc
[batch]: http://lists.gnu.org/archive/html/emacs-pretest-bug/2005-11/msg00320.html
