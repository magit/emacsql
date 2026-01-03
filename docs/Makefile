-include ../config.mk
include ../default.mk

.PHONY: stats

stats:
	@printf "Generating statistics...\n"
	@$(GITSTATS) $(GITSTATS_ARGS) $(TOP) $(GITSTATS_DIR)

stats-upload:
	@printf "Uploading statistics...\n"
	@aws s3 sync $(GITSTATS_DIR) $(S3_BUCKET)/stats/$(PKG)
	@printf "Uploaded to $(S3_BUCKET)/stats/$(PKG)\n"
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation \
	--distribution-id $(CFRONT_DIST) --paths "/stats/*" > /dev/null

CLEAN = stats

clean:
	@printf " Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
