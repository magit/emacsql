-include config.mk
include default.mk

.PHONY: test

all: lisp

help:
	$(info make all      -- Build lisp)
	$(info make lisp     -- Build lisp)
	$(info make redo     -- Build lisp from scratch)
	$(info make test     -- Run tests)
	$(info make clean    -- Remove built files)
	@printf "\n"

redo: clean lisp
lisp: $(ELCS) autoloads check-declare

autoloads: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

clean: clean-lisp clean-docs clean-test
clean-lisp:
	@printf " Cleaning *...\n"
	@rm -rf $(ELCS) $(PKG)-autoloads.el
clean-docs:
	@$(MAKE) -C docs clean
clean-test:
	@$(MAKE) -C test clean

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --load autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"

stats:
	@$(MAKE) -C docs stats
stats-upload:
	@$(MAKE) -C docs stats-upload

test: lisp
	@$(MAKE) -C test test

test-interactive:
	@$(MAKE) -C test test-interactive
