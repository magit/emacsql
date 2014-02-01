ifeq ($(OS),Windows_NT)
    OSTUPLE = x86-windows
    BINARY  = emacsql-sqlite-$(OSTUPLE).exe
else
    PART_ARCH = $(shell uname -m)
    PART_OS   = $(shell uname -ms | tr '[A-Z]' '[a-z]')
    OSTUPLE   = $(PART_ARCH)-$(PART_OS)
    BINARY    = emacsql-sqlite-$(OSTUPLE)
    LDLIBS    = -ldl
endif

CFLAGS  = -O2 --std=c99 -W -Wall -Wextra
CFLAGS += -DSQLITE_THREADSAFE=0
CFLAGS += -DSQLITE_DEFAULT_FOREIGN_KEYS=1
CLFAGS += -DSQLITE_ENABLE_FTS4
CLFAGS += -DSQLITE_ENABLE_FTS3_PARENTHESIS
CLFAGS += -DSQLITE_SOUNDEX

.PHONY : clean install

install : $(BINARY)
	mkdir -p ../bin/
	cp $< ../bin/

$(BINARY) : emacsql.o sqlite3.o
	$(CC) $(LDLIBS) $(LDFLAGS) -o $@ $^

clean :
	$(RM) *.o $(BINARY) ../bin/$(BINARY)
