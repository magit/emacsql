EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -batch -Q -L .

PACKAGE := emacsql
VERSION := $(shell $(CASK) version)

EL = emacsql-compiler.el emacsql.el emacsql-sqlite.el emacsql-psql.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

.PHONY : all compile package test clean

all : test

.cask : Cask
	cask install
	touch .cask

compile: .cask $(ELC)

package : $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-pkg.el : Cask
	$(CASK) package

$(PACKAGE)-$(VERSION).tar : $(EL) $(PACKAGE)-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

test: compile $(PACKAGE)-tests.elc
	$(BATCH) -l $(PACKAGE)-tests.elc -f ert-run-tests-batch

clean:
	$(RM) *.tar *.elc $(PACKAGE)-pkg.el

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<
