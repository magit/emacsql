-include ../config.mk
include ../default.mk

LOAD_PATH += -L ../

TEST_ELS  = emacsql-compiler-tests.el
TEST_ELS += emacsql-external-tests.el
TEST_ELCS = $(TEST_ELS:.el=.elc)

test: lisp
	@printf "Running compiler tests...\n"
	@$(EMACS_BATCH) -l emacsql-compiler-tests.elc -f ert-run-tests-batch-and-exit
	@printf "Running connector tests...\n"
	@$(EMACS_BATCH) -l emacsql-external-tests.elc -f ert-run-tests-batch-and-exit

test-interactive:
	@$(EMACS_INTR) $(addprefix -l ,$(TEST_ELS)) --eval "(ert t)"

lisp: $(addprefix ../,$(ELCS)) $(TEST_ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

clean:
	@printf " Cleaning test/*...\n"
	@rm -rf $(TEST_ELCS)
