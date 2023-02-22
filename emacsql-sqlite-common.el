;;; emacsql-sqlite-common.el --- Code used by multiple SQLite back-ends  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library contains code that is used by multiple SQLite back-ends.

;;; Code:

;;; Constants

(defconst emacsql-sqlite-reserved
  '( ABORT ACTION ADD AFTER ALL ALTER ANALYZE AND AS ASC ATTACH
     AUTOINCREMENT BEFORE BEGIN BETWEEN BY CASCADE CASE CAST CHECK
     COLLATE COLUMN COMMIT CONFLICT CONSTRAINT CREATE CROSS
     CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP DATABASE DEFAULT
     DEFERRABLE DEFERRED DELETE DESC DETACH DISTINCT DROP EACH ELSE END
     ESCAPE EXCEPT EXCLUSIVE EXISTS EXPLAIN FAIL FOR FOREIGN FROM FULL
     GLOB GROUP HAVING IF IGNORE IMMEDIATE IN INDEX INDEXED INITIALLY
     INNER INSERT INSTEAD INTERSECT INTO IS ISNULL JOIN KEY LEFT LIKE
     LIMIT MATCH NATURAL NO NOT NOTNULL NULL OF OFFSET ON OR ORDER
     OUTER PLAN PRAGMA PRIMARY QUERY RAISE RECURSIVE REFERENCES REGEXP
     REINDEX RELEASE RENAME REPLACE RESTRICT RIGHT ROLLBACK ROW
     SAVEPOINT SELECT SET TABLE TEMP TEMPORARY THEN TO TRANSACTION
     TRIGGER UNION UNIQUE UPDATE USING VACUUM VALUES VIEW VIRTUAL WHEN
     WHERE WITH WITHOUT)
  "List of all of SQLite's reserved words.
Also see http://www.sqlite.org/lang_keywords.html.")

(defconst emacsql-sqlite-error-codes
  '((1  SQLITE_ERROR      emacsql-error      "SQL logic error")
    (2  SQLITE_INTERNAL   emacsql-internal   nil)
    (3  SQLITE_PERM       emacsql-access     "access permission denied")
    (4  SQLITE_ABORT      emacsql-error      "query aborted")
    (5  SQLITE_BUSY       emacsql-locked     "database is locked")
    (6  SQLITE_LOCKED     emacsql-locked     "database table is locked")
    (7  SQLITE_NOMEM      emacsql-memory     "out of memory")
    (8  SQLITE_READONLY   emacsql-access     "attempt to write a readonly database")
    (9  SQLITE_INTERRUPT  emacsql-error      "interrupted")
    (10 SQLITE_IOERR      emacsql-access     "disk I/O error")
    (11 SQLITE_CORRUPT    emacsql-corruption "database disk image is malformed")
    (12 SQLITE_NOTFOUND   emacsql-error      "unknown operation")
    (13 SQLITE_FULL       emacsql-access     "database or disk is full")
    (14 SQLITE_CANTOPEN   emacsql-access     "unable to open database file")
    (15 SQLITE_PROTOCOL   emacsql-access     "locking protocol")
    (16 SQLITE_EMPTY      emacsql-corruption nil)
    (17 SQLITE_SCHEMA     emacsql-error      "database schema has changed")
    (18 SQLITE_TOOBIG     emacsql-error      "string or blob too big")
    (19 SQLITE_CONSTRAINT emacsql-constraint "constraint failed")
    (20 SQLITE_MISMATCH   emacsql-error      "datatype mismatch")
    (21 SQLITE_MISUSE     emacsql-error      "bad parameter or other API misuse")
    (22 SQLITE_NOLFS      emacsql-error      "large file support is disabled")
    (23 SQLITE_AUTH       emacsql-access     "authorization denied")
    (24 SQLITE_FORMAT     emacsql-corruption nil)
    (25 SQLITE_RANGE      emacsql-error      "column index out of range")
    (26 SQLITE_NOTADB     emacsql-corruption "file is not a database")
    (27 SQLITE_NOTICE     emacsql-warning    "notification message")
    (28 SQLITE_WARNING    emacsql-warning    "warning message"))
  "Alist mapping SQLite error codes to EmacSQL conditions.
Elements have the form (ERRCODE SYMBOLIC-NAME EMACSQL-ERROR
ERRSTR).  Also see https://www.sqlite.org/rescode.html.")

(provide 'emacsql-sqlite-common)

;;; emacsql-sqlite-common.el ends here
