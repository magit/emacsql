EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -batch -Q -L .

PACKAGE := emacsql
VERSION := $(shell $(CASK) version)

EL = emacsql-compiler.el emacsql-system.el emacsql.el \
     emacsql-sqlite.el emacsql-psql.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

.PHONY : all binary compile package test clean distclean

all : test

.cask : Cask
	cask install
	touch .cask

binary :
	$(MAKE) -C sqlite

compile: .cask $(ELC) binary

package : $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-pkg.el : Cask
	$(CASK) package

$(PACKAGE)-$(VERSION).tar : $(EL) $(PACKAGE)-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

test: compile $(PACKAGE)-tests.elc
	$(BATCH) -l $(PACKAGE)-tests.elc -f ert-run-tests-batch

clean :
	$(RM) *.tar *.elc $(PACKAGE)-pkg.el

distclean :
	$(MAKE) -C sqlite clean

%.elc : %.el
	$(BATCH) -f batch-byte-compile $<
