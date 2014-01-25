PACKAGE  = emacsql
VERSION := $(word 1,$(subst -, ,$(shell git describe)))

EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile

EL = emacsql-reap.el emacsql-compiler.el emacsql.el emacsql-sqlite.el \
     emacsql-psql.el emacsql-tests.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

.PHONY : all compile package test clean

all : test

compile: $(ELC)

package : $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-$(VERSION).tar : $(EL) $(PACKAGE)-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

test: compile
	$(BATCH) -l $(PACKAGE)-tests.elc -f ert-run-tests-batch

clean:
	$(RM) *.tar $(ELC)

%.elc: %.el
	$(COMPILE) $<
