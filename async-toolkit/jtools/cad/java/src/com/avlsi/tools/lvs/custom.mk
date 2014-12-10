
CURR_RESULT_FILES := $(CURR_TARGET_DIR)/jlvs.sh $(CURR_TARGET_DIR)/prs2net.sh $(CURR_RESULT_FILES)

$(CURR_TARGET_DIR)/jlvs.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.lvs.LVS/"  >$@

$(CURR_TARGET_DIR)/prs2net.sh: \
	$(CURR_PROJECT_DIR)/../../../../../scripts/fulcrum-java.sh.template
	cat $< | $(GNUSED) -e "s/\\\$$appname\\\$$/com.avlsi.tools.lvs.PrsToNet/"  >$@
