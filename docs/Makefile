-include ../config.mk
include ../default.mk

.PHONY: stats

STAT_DOMAIN = stats.$(DOMAIN)
STAT_TARGET = $(subst .,_,$(STAT_DOMAIN)):$(PKG)/

stats:
	@printf "Generating statistics...\n"
	@$(GITSTATS) $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@$(RCLONE) sync $(RCLONE_ARGS) stats $(STAT_TARGET)

CLEAN = stats

clean:
	@printf " Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
