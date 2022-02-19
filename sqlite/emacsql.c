/* This is free and unencumbered software released into the public domain.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sqlite3.h"

static int
send_error(int code, const char *message)
{
    if (printf("error %d \"", code) < 0) {
        return 0;
    }

    for (;;) {
        size_t n = strcspn(message, "\"\\");
        if (n && !fwrite(message, n, 1, stdout)) {
            return 0;
        }

        message += n;
        if (!*message) {
            return fwrite("\"\n", 2, 1, stdout);
        }

        char esc[2] = {'\\', *message++};
        if (!fwrite(esc, 2, 1, stdout)) {
            return 0;
        }
    }
}

int
main(int argc, char **argv)
{
    char *buf = 0;
    size_t buflen = 0;

#ifdef _WIN32
    int _setmode(int, int);
    _setmode(0, 0x8000);
    _setmode(1, 0x8000);
    setvbuf(stderr, 0, _IONBF, 0);
#endif

    if (argc != 2) {
        static const char usage[] = "usage: emacsql-sqlite DBFILE\n";
        fwrite(usage, sizeof(usage)-1, 1, stderr);
        goto fail;
    }
    char *dbfile = argv[1];

    sqlite3* db;
    int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    if (sqlite3_open_v2(dbfile, &db, flags, 0) != SQLITE_OK) {
        send_error(sqlite3_errcode(db), sqlite3_errmsg(db));
        goto fail;
    }

    for (;;) {
        if (!fwrite("#\n", 2, 1, stdout) || fflush(stdout)) {
            goto fail;
        }

        // format: BASE-10-LENGTH SP SQL LF
        size_t sqllen = 0;
        for (int first = 1;; first = 0) {
            int c = fgetc(stdin);
            switch (c) {
            default:
                send_error(SQLITE_ERROR, "emacsql parse error");
                goto fail;
            case EOF:
                if (first && !ferror(stdin) && !fflush(stdout)) {
                    free(buf);
                    return 0;
                }
                goto fail;
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                if (sqllen > ((size_t)-1)/20) {
                    send_error(SQLITE_NOMEM, "buffer too large");
                    goto fail;
                }
                sqllen = sqllen*10 + c - '0';
                continue;
            case ' ':
                break;
            }
            break;
        }

        if (buflen < sqllen+1) {
            void *p = realloc(buf, sqllen+1);
            if (!p) {
                send_error(SQLITE_NOMEM, "out of memory");
                goto fail;
            }
            buf = p;
            buflen = sqllen+1;
        }

        if (!fread(buf, sqllen+1, 1, stdin)) {
            goto fail;
        }

        sqlite3_stmt *stmt = 0;
        if (sqlite3_prepare_v2(db, buf, sqllen, &stmt, 0) != SQLITE_OK) {
            if (!send_error(sqlite3_errcode(db), sqlite3_errmsg(db))) {
                goto fail;
            }
            continue;
        }

        if (fputc('(', stdout) == EOF) {
            goto fail;
        }

        int ncolumns = sqlite3_column_count(stmt);
        for (int first = 1; sqlite3_step(stmt) == SQLITE_ROW; first = 0) {
            if (first) {
                if (fputc('(', stdout) == EOF) {
                    goto fail;
                }
            } else if (!fwrite("\n (", 3, 1, stdout)) {
                goto fail;
            }

            for (int i = 0; i < ncolumns; i++) {
                if (i > 0) {
                    if (fputc(' ', stdout) == EOF) {
                        goto fail;
                    }
                }

                switch (sqlite3_column_type(stmt, i)) {
                case SQLITE_INTEGER:
                    printf("%lld", (long long)sqlite3_column_int64(stmt, i));
                    break;
                case SQLITE_FLOAT:
                    printf("%#.17g", sqlite3_column_double(stmt, i));
                    break;
                case SQLITE_NULL:
                    fwrite("nil", 3, 1, stdout);
                    break;
                case SQLITE_TEXT:
                    fwrite(sqlite3_column_text(stmt, i), 1,
                           sqlite3_column_bytes(stmt, i), stdout);
                    break;
                case SQLITE_BLOB:
                    fwrite("nil", 3, 1, stdout);
                    break;
                }
                if (ferror(stdout)) {
                    goto fail;
                }
            }

            if (fputc(')', stdout) == EOF) {
                goto fail;
            }
        }

        if (fputc(')', stdout) == EOF) {
            goto fail;
        }

        if (sqlite3_finalize(stmt) != SQLITE_OK) {
            if (!send_error(sqlite3_errcode(db), sqlite3_errmsg(db))) {
                goto fail;
            }
        } else {
            static const char success[] = "success\n";
            if (!fwrite(success, sizeof(success)-1, 1, stdout)) {
                goto fail;
            }
        }
    }

fail:
    free(buf);
    return 1;
}
