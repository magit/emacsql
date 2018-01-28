# Clone the dependencies of this package in sibling directories:
#     $ git clone https://github.com/cbbrowne/pg.el ../pg
#
# Or set LDFLAGS to point at these packages elsewhere:
#     $ make LDFLAGS='-L path/to/pg'

.POSIX:
.SUFFIXES: .el .elc
EMACS   = emacs
LDFLAGS = -L ../pg
BATCH   = $(EMACS) -batch -Q -L . -L tests $(LDFLAGS)

EL = emacsql-compiler.el \
     emacsql.el \
     emacsql-sqlite.el \
     emacsql-psql.el \
     emacsql-mysql.el \
     emacsql-pg.el
ELC = $(EL:.el=.elc)
TEST_EL = \
    tests/emacsql-compiler-tests.el \
    tests/emacsql-external-tests.el \
    tests/emacsql-tests.el
TEST_ELC = $(TEST_EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

all: test

binary: sqlite/emacsql-sqlite
sqlite/emacsql-sqlite:
	$(MAKE) -C sqlite

compile: $(ELC)

check: test
test: compile $(TEST_ELC)
	$(BATCH) -l tests/emacsql-tests.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC) $(TEST_ELC)

distclean: clean
	$(MAKE) -C sqlite clean

.el.elc:
	$(BATCH) -f batch-byte-compile $<
