BINARY  = emacsql-sqlite-$(shell uname -sm | tr '[A-Z]' '[a-z]' | tr ' ' '-')
LDLIBS  = -ldl
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
