CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(CURR_TARGET_DIR)/synthesis.sh

$(CURR_TARGET_DIR)/synthesis.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.synthesis.GenerateLogicLibrary/"  >$@

$(CURR_TARGET_DIR)/generatelib.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.synthesis.GenerateLib/"  >$@
