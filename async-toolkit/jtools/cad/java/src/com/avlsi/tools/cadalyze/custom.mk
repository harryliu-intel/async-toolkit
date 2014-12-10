CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(CURR_TARGET_DIR)/cadalyze.sh

$(CURR_TARGET_DIR)/cadalyze.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.cadalyze.Cadalyze/"  >$@
