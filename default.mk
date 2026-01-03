TOP := $(dir $(lastword $(MAKEFILE_LIST)))

DOMAIN ?= magit.vc

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

ifeq ($(CI), true)
# Workaround for bug#58252 on Emacs 28.x.
override EMACS_ARGS += --eval "(setq byte-compile-docstring-max-column 120)"
else
EMACS_ARGS ?=
endif

EMACS       ?= emacs
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(LOAD_PATH)

ifeq ($(CI), true)
override GITSTATS = ../_gitstats/gitstats
endif
GITSTATS      ?= gitstats
GITSTATS_DIR  ?= stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css -c max_authors=999

CFRONT_DIST ?= E2LUHBKU1FBV02
S3_BUCKET   ?= s3://$(DOMAIN)

ifdef NIX_PATH
export SQLITE3_API_BUILD_COMMAND = nix-shell -p sqlite.dev --run "make all"
endif
