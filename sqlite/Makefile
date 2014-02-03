EMACS    ?= emacs
GETTUPLE  = -l ../emacsql-system.el -f emacsql-system-print-tuple
OSTUPLE  ?= $(shell $(EMACS) -Q -batch $(GETTUPLE))
BINARY    = emacsql-sqlite-$(OSTUPLE)

ifneq (,$(findstring windows, $(OSTUPLE)))
    ## Windows
    BINARY := $(BINARY).exe
else
    ## Unix-like
    LDLIBS  = -ldl
endif

CFLAGS  = -O2 -W -Wall -Wextra
CFLAGS += -DSQLITE_THREADSAFE=0
CFLAGS += -DSQLITE_DEFAULT_FOREIGN_KEYS=1
CFLAGS += -DSQLITE_ENABLE_FTS4
CFLAGS += -DSQLITE_ENABLE_FTS3_PARENTHESIS
CFLAGS += -DSQLITE_SOUNDEX

.PHONY : clean install

install : ../bin/$(BINARY)

../bin/$(BINARY) : $(BINARY)
	mkdir -p ../bin/
	cp $< ../bin/

$(BINARY) : emacsql.o sqlite3.o
	$(CC) $(LDLIBS) $(LDFLAGS) -o $@ $^

clean :
	$(RM) *.o $(BINARY) emacsql-sqlite-* ../bin/$(BINARY)
