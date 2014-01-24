PACKAGE  = emacsql

EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile
TEST    := $(BATCH) -l $(PACKAGE)-tests.elc -f ert-run-tests-batch

EL = emacsql.el emacsql-sqlite.el $(PACKAGE)-tests.el

ELC = $(EL:.el=.elc)

.PHONY : all compile test clean

all : test

compile: $(ELC)

test: compile
	$(TEST)

clean:
	$(RM) *.elc

%.elc: %.el
	$(COMPILE) $<
