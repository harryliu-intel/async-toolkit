
CURR_RESULT_FILES := $(CURR_TARGET_DIR)/csp_coverage.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/csp_coverage.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.csp.coverage.CoverageTool/"  >$@
