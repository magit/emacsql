-include .config.mk

PKG = emacsql

ELS   = $(PKG)-compiler.el
ELS  += $(PKG).el
ELS  += $(PKG)-sqlite.el
ELS  += $(PKG)-sqlite-builtin.el
ELS  += $(PKG)-sqlite-module.el
ELS  += $(PKG)-mysql.el
ELS  += $(PKG)-psql.el
ELS  += $(PKG)-pg.el
ELCS  = $(ELS:.el=.elc)

TEST_ELS  = test/emacsql-compiler-tests.el
TEST_ELS += test/emacsql-external-tests.el
TEST_ELCS = $(TEST_ELS:.el=.elc)

DEPS  = pg
DEPS += peg
DEPS += sqlite3

LOAD_PATH ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH += -L .
LOAD_PATH += -L ./test

ifdef NIX_PATH
export SQLITE3_API_BUILD_COMMAND = nix-shell -p sqlite.dev --run "make all"
endif

ifeq ($(CI), true)
# Workaround for bug#58252 on Emacs 28.x.
override EMACS_ARGS += --eval "(setq byte-compile-docstring-max-column 120)"
else
EMACS_ARGS ?=
endif

EMACS       ?= emacs
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(LOAD_PATH)

.PHONY: test stats

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

CLEAN = $(ELCS) $(TEST_ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)

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

test: all $(TEST_ELCS)
	@printf "Running compiler tests...\n"
	@$(EMACS_BATCH) -L tests \
	-l test/emacsql-compiler-tests.elc -f ert-run-tests-batch-and-exit
	@printf "Running connector tests...\n"
	@$(EMACS_BATCH) -L tests \
	-l test/emacsql-external-tests.elc -f ert-run-tests-batch-and-exit

ifeq ($(CI), true)
override GITSTATS = ../_gitstats/gitstats
endif
GITSTATS      ?= gitstats
GITSTATS_DIR  ?= stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999

DOMAIN      ?= magit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02
S3_BUCKET   ?= s3://$(DOMAIN)

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
