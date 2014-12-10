
CURR_RESULT_FILES := $(CURR_TARGET_DIR)/slacker.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/slacker.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.slacker.Slacker/"  >$@
