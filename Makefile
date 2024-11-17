-include .config.mk

PKG = emacsql

ELS   = $(PKG)-compiler.el
ELS  += $(PKG).el
ELS  += $(PKG)-sqlite.el
ELS  += $(PKG)-sqlite-common.el
ELS  += $(PKG)-sqlite-builtin.el
ELS  += $(PKG)-sqlite-module.el
ELS  += $(PKG)-mysql.el
ELS  += $(PKG)-psql.el
ELS  += $(PKG)-pg.el
ELCS  = $(ELS:.el=.elc)

TEST_ELS  = tests/emacsql-compiler-tests.el
TEST_ELS += tests/emacsql-external-tests.el
TEST_ELCS = $(TEST_ELS:.el=.elc)

DEPS  = pg
DEPS += peg
DEPS += sqlite3

EMACS      ?= emacs

ifeq ($(CI), true)
# Workaround for bug#58252 on Emacs 28.x.
override EMACS_ARGS += --eval "(setq byte-compile-docstring-max-column 120)"
else
EMACS_ARGS ?=
endif

ifdef NIX_PATH
export SQLITE3_API_BUILD_COMMAND = nix-shell -p sqlite.dev --run "make all"
endif

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .
LOAD_PATH  += -L ./tests

all: lisp

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make redo         - re-generate byte-code and autoloads)
	$(info make test         - run tests)
	$(info make clean        - remove byte-code and autoloads)
	@printf "\n"

redo: clean lisp
lisp: $(ELCS) loaddefs

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	--eval "(check-declare-directory default-directory)"

CLEAN  = $(ELCS) $(TEST_ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload -l cl-lib --eval "\
(let ((file (expand-file-name \"$@\"))\
      (autoload-timestamps nil) \
      (backup-inhibited t)\
      (version-control 'never)\
      (coding-system-for-write 'utf-8-emacs-unix))\
  (write-region (autoload-rubric file \"package\" nil) nil file nil 'silent)\
  (cl-letf (((symbol-function 'progress-reporter-do-update) (lambda (&rest _)))\
	    ((symbol-function 'progress-reporter-done) (lambda (_))))\
    (let ((generated-autoload-file file))\
      (update-directory-autoloads default-directory))))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"

test: all $(TEST_ELCS)
	@printf "Running compiler tests...\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -L tests \
	-l tests/emacsql-compiler-tests.elc -f ert-run-tests-batch-and-exit
	@printf "Running connector tests...\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -L tests \
	-l tests/emacsql-external-tests.elc -f ert-run-tests-batch-and-exit

ifeq ($(CI), true)
override GITSTATS = ../_gitstats/gitstats
endif
GITSTATS      ?= gitstats
GITSTATS_DIR  ?= stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999

DOMAIN      ?= magit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02
S3_BUCKET   ?= s3://$(DOMAIN)

.PHONY: stats
stats:
	@printf "Generating statistics\n"
	@$(GITSTATS) $(GITSTATS_ARGS) . $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@aws s3 sync $(GITSTATS_DIR) $(S3_BUCKET)/stats/$(PKG)
	@printf "Uploaded to $(S3_BUCKET)/stats/$(PKG)\n"
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/stats/*" > /dev/null
