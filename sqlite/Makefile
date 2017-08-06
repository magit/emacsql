.POSIX:
EMACS    = emacs
BINARY   = emacsql-sqlite
GETTUPLE = -l ../emacsql-system.el -f emacsql-system-print-tuple
SUFFIX   = $$($(EMACS) -Q -batch $(GETTUPLE))
LDLIBS   = -ldl
CFLAGS   = -O2 -W -Wall -Wextra \
    -DSQLITE_THREADSAFE=0 \
    -DSQLITE_DEFAULT_FOREIGN_KEYS=1 \
    -DSQLITE_ENABLE_FTS4 \
    -DSQLITE_ENABLE_FTS3_PARENTHESIS \
    -DSQLITE_SOUNDEX

emacsql-sqlite: emacsql.c sqlite3.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ emacsql.c sqlite3.c $(LDLIBS)

install: emacsql-sqlite
	mkdir -p ../bin/
	cp emacsql-sqlite ../bin/emacsql-sqlite-$(SUFFIX)

clean:
	rm -f emacsql-sqlite ../bin/emacsql-sqlite-$(SUFFIX)
