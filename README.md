# Emacsql

Emacsql is a high-level Emacs Lisp front-end for SQLite. It's
currently a work in progress.

It works by keeping a `sqlite3` inferior process running (a
"connection") for interacting with the back-end database. Connections
are automatically cleaned up if they are garbage collected. All
requests are synchronous.

Any [readable lisp value][readable] can be stored as a value in
Emacsql, including numbers, strings, symbols, lists, vectors, and
closures. Emacsql has no concept of "TEXT" values; it's all just lisp
objects.

Requires Emacs 24 or later.

## Example Usage

```el
(defvar db (emacsql-connect "/var/lib/company.db"))

;; Create a table. A table identifier can be any kind of lisp value.
(emacsql db [:create-table people [name id salary]])

;; Or optionally provide column constraints.
(emacsql db [:create-table people [name (id integer :unique) (salary float)]])

;; Insert some data:
(emacsql db [:insert :into people
             :values (["Jeff"  1000 60000.0] ["Susan" 1001 64000.0])])

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

## Schema

A table schema is a vector of column specification. A column
identifier is a symbol and a specification can either be just this
symbol or it can include constraints, such as type and uniqueness.
Because Emacsql stores entire lisp objects as values, the only
relevant types are `integer`, `float`, and `object` (default).

Additional constraints include `:primary` (aka `PRIMARY KEY`),
`:unique` (aka `UNIQUE`), `:non-nil` (aka `NOT NULL`).

```el
;; Example schema:
[name (badge-no integer :primary :unique) address]
```

The lisp object `nil` corresponds 1:1 with `NULL` in the database.

## Operators

Emacsql currently supports the following expression operators, named
exactly like so in a structured Emacsql statement.

    *     /     %     +     -     <<    >>    &
    |     <     <=    >     >=    =     !=
    is    like  glob  and   or    in

The `<=` and `>=` operators accept 2 or 3 operands, transforming into
a SQL `_ BETWEEN _ AND _` operator as appropriate.

With `glob` and `like` keep in mind that they're matching the
*printed* representations of these values, even if the value is a
string.

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

#### :create-table `<table>` `<schema>`

Provides `CREATE TABLE`.

```el
[:create-table employees [name (id integer :primary) (salary float)]]
```

#### :drop-table `<table>`

Provides `DROP TABLE`.

```el
[:drop-table employees]
```

#### :select `<column-spec>`

Provides `SELECT`. `column-spec` can be a `*` symbol or a vector of
column identifiers, optionally as expressions.

```el
[:select [name (/ salary 52)] ...]
```

#### :from `<table>`

Provides `FROM`.

```el
[... :from employees]
```

#### :into `<table>`

Provides `INTO`.

```el
[... :into employees]
```

#### :delete

Provides `DELETE`.

```el
[:delete :from employees :where ...]
```

#### :insert

Provides `INSERT`.

```el
[:insert :into employees ...]
```

#### :replace

Provides `REPLACE`.

```el
[:replace :into employees ...]
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

### Templates

To make statement compilation faster, and to avoid making you build up
statements dynamically, you can insert `$n` "variables" in place of
identifiers and values. These refer to argument positions after the
statement in the `emacsql` function, 1-indexed.

    (emacsql db [:select * :from $1 :where (> salary $2)] 'employees 50000)

## Limitations

Emacsql is *not* intended to play well with other programs accessing
the SQLite database. Non-numeric values are are stored encoded as
s-expressions TEXT values. This avoids ambiguities in parsing output
from the command line and allows for storage of Emacs richer data
types. This is a high-performance database specifically for Emacs.

## See Also

 * [SQLite Documentation](https://www.sqlite.org/docs.html)


[readable]: http://nullprogram.com/blog/2013/12/30/#almost_everything_prints_readably
