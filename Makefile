EMACS   = emacs
CASK    = cask
BATCH   = $(CASK) exec $(EMACS) -batch -Q -L . -L tests

EL = emacsql-compiler.el \
     emacsql-system.el \
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

.cask: Cask
	cask install
	touch .cask

binary:
	$(MAKE) -C sqlite

compile: .cask $(ELC)

package: emacsql-$(VERSION).tar

test: compile $(TEST_ELC)
	$(BATCH) -l tests/emacsql-tests.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC) $(TEST_ELC)

distclean: clean
	rm -f bin/*
	$(MAKE) -C sqlite clean

.SUFFIXES: .el .elc

.el.elc:
	$(BATCH) -f batch-byte-compile $<
