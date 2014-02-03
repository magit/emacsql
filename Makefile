EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -batch -Q -L . -L tests

PACKAGE := emacsql
VERSION := $(shell $(CASK) version)

EL = emacsql-compiler.el emacsql-system.el emacsql.el \
     emacsql-sqlite.el emacsql-psql.el emacsql-mysql.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

TEST_EL  = $(wildcard tests/*.el)
TEST_ELC = $(TEST_EL:.el=.elc)

.PHONY : all binary compile package test clean distclean

all : test

.cask : Cask
	cask install
	touch .cask

binary :
	$(MAKE) -C sqlite

compile: .cask $(ELC) binary

package : compile $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-pkg.el : Cask
	$(CASK) package

$(PACKAGE)-$(VERSION).tar : $(PACKAGE)-pkg.el $(EL) bin/ $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

test: compile $(TEST_ELC)
	$(BATCH) -l tests/$(PACKAGE)-tests.elc -f ert-run-tests-batch

clean :
	$(RM) *.tar *.elc tests/*.elc $(PACKAGE)-pkg.el

distclean : clean
	$(MAKE) -C sqlite clean

%.elc : %.el
	$(BATCH) -f batch-byte-compile $<
